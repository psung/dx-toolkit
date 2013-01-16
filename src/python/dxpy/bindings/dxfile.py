'''
DXFile Handler
**************

This remote file handler is a Python file-like object.
'''

import os, logging, traceback
import cStringIO as StringIO
import concurrent.futures
from dxpy.bindings import *
import dxpy.utils

if dxpy.snappy_available:
    import snappy

# TODO: adaptive buffer size
DEFAULT_BUFFER_SIZE = 1024*1024*32
if dxpy.JOB_ID:
    # Increase HTTP request buffer sizes when we are running within the
    # platform.
    DEFAULT_BUFFER_SIZE = 1024*1024*128

class DXFile(DXDataObject):
    '''
    :param dxid: Object ID
    :type dxid: string
    :param project: Project ID
    :type project: string
    :param mode: One of "r", "w", or "a" for read, write, and append modes, respectively
    :type mode: string

    Remote file object handler.

    .. automethod:: _new

    '''

    _class = "file"

    _describe = staticmethod(dxpy.api.fileDescribe)
    _add_types = staticmethod(dxpy.api.fileAddTypes)
    _remove_types = staticmethod(dxpy.api.fileRemoveTypes)
    _get_details = staticmethod(dxpy.api.fileGetDetails)
    _set_details = staticmethod(dxpy.api.fileSetDetails)
    _set_visibility = staticmethod(dxpy.api.fileSetVisibility)
    _rename = staticmethod(dxpy.api.fileRename)
    _set_properties = staticmethod(dxpy.api.fileSetProperties)
    _add_tags = staticmethod(dxpy.api.fileAddTags)
    _remove_tags = staticmethod(dxpy.api.fileRemoveTags)
    _close = staticmethod(dxpy.api.fileClose)
    _list_projects = staticmethod(dxpy.api.fileListProjects)

    _http_threadpool = None
    _http_threadpool_size = NUM_HTTP_THREADS

    @classmethod
    def set_http_threadpool_size(cls, num_threads):
        cls._http_threadpool_size = num_threads

    def __init__(self, dxid=None, project=None, mode=None,
                 read_buffer_size=DEFAULT_BUFFER_SIZE, write_buffer_size=DEFAULT_BUFFER_SIZE):
        DXDataObject.__init__(self, dxid=dxid, project=project)
        if mode is None:
            self._close_on_exit = True
        else:
            if mode not in ['r', 'w', 'a']:
                raise ValueError("mode must be one of 'r', 'w', or 'a'")
            self._close_on_exit = (mode == 'w')

        self._read_buf = StringIO.StringIO()
        self._write_buf = StringIO.StringIO()
        self._num_uploaded_parts = 0

        if write_buffer_size < 5*1024*1024:
            raise DXFileError("Write buffer size must be at least 5 MB")

        self._read_bufsize = read_buffer_size
        self._write_bufsize = write_buffer_size

        self._download_url, self._download_url_expires = None, None
        self._request_iterator, self._response_iterator = None, None
        self._http_threadpool_futures = set()

        # Initialize state
        self._pos = 0
        self._file_length = None
        self._cur_part = 1
        self._num_uploaded_parts = 0

    def _new(self, dx_hash, media_type=None, **kwargs):
        """
        :param dx_hash: Standard hash populated in :func:`dxpy.bindings.DXDataObject.new()` containing attributes common to all data object classes.
        :type dx_hash: dict
        :param media_type: Internet Media Type
        :type media_type: string

        Creates a new remote file with media type *media_type*, if given.

        """

        if media_type is not None:
            dx_hash["media"] = media_type

        resp = dxpy.api.fileNew(dx_hash, **kwargs)
        self.set_ids(resp["id"], dx_hash["project"])

    def __enter__(self):
        return self

    def __exit__(self, type, value, traceback):
        self.flush()
        if self._close_on_exit and self._get_state() == "open":
            self.close()

    def __del__(self):
        '''
        Exceptions raised here in the destructor are IGNORED by Python! We will try and flush data
        here just as a safety measure, but you should not rely on this to flush your data! We will
        be really grumpy and complain if we detect unflushed data here.

        Use a context manager or flush the object explicitly to avoid this.

        In addition, when this is triggered by interpreter shutdown, the thread pool is not
        available, and we will wait for the request queue forever. In this case, we must revert to
        synchronous, in-thread flushing. We don't know how to detect this condition, so we'll use
        that for all destructor events.

        Neither this nor context managers are compatible with kwargs pass-through (so e.g. no
        custom auth).
        '''
        if self._write_buf.tell() > 0 or len(self._http_threadpool_futures) > 0:
            print >> sys.stderr, "=== WARNING! ==="
            print >> sys.stderr, "There is still unflushed data in the destructor of a DXFile object!"
            print >> sys.stderr, "We will attempt to flush it now, but if an error were to occur, we could not report it back to you."
            print >> sys.stderr, "Your program could fail to flush the data but appear to succeed."
            print >> sys.stderr, "Instead, please call flush() or close(), or use the context managed version (e.g., with open_dxfile(ID, mode='w') as f:)"
        try:
            self.flush(multithread=False)
        except Exception as e:
            print >> sys.stderr, "=== Exception occurred while flushing accumulated file data for %r" % (self._dxid,)
            traceback.print_exception(*sys.exc_info())
            raise

    def __iter__(self):
        buffer = self.read(self._read_bufsize)
        done = False
        while not done:
            if "\n" in buffer:
                lines = buffer.splitlines()
                for i in range(len(lines) - 1):
                    yield lines[i]
                buffer = lines[len(lines) - 1]
            else:
                more = self.read(self._read_bufsize)
                if more == "":
                    done = True
                else:
                    buffer = buffer + more
        if buffer:
            yield buffer

    def set_ids(self, dxid, project=None):
        '''
        :param dxid: Object ID
        :type dxid: string
        :param project: Project ID
        :type project: string

        Discards the currently stored ID and associates the handler with
        *dxid*. As a side effect, it also flushes the buffer for the
        previous file object if the buffer is nonempty.
        '''
        if self._dxid is not None:
            self.flush()

        DXDataObject.set_ids(self, dxid, project)

        # Reset state
        self._pos = 0
        self._file_length = None
        self._cur_part = 1
        self._num_uploaded_parts = 0

    def seek(self, offset):
        '''
        :param offset: Position in the file to seek to
        :type offset: integer

        Seeks to *offset* bytes from the beginning of the file.  This
        is a no-op if the file is open for writing.

        '''
        orig_pos = self._pos
        self._pos = offset

        in_buf = False
        orig_buf_pos = self._read_buf.tell()
        if offset < orig_pos:
            if orig_buf_pos > orig_pos - offset:
                # offset is less than original position but within the buffer
                in_buf = True
        else:
            buf_len = dxpy.utils.string_buffer_length(self._read_buf)
            if buf_len - orig_buf_pos > offset - orig_pos:
                # offset is greater than original position but within the buffer
                in_buf = True

        if in_buf: # offset is within the buffer
            self._read_buf.seek(orig_buf_pos - orig_pos + offset)
        else: # offset is outside the buffer - reset buffer and queues. This is the failsafe behavior
            self._read_buf = StringIO.StringIO()
            # TODO: if the offset is within the next response(s), don't throw out the queues
            self._request_iterator, self._response_iterator = None, None

    def tell(self):
        '''
        Returns the current position of the file read cursor.

        Warning: Because of buffering semantics, this value will **not** be accurate when using the line iterator form
        (`for line in file`).
        '''
        return self._pos

    def flush(self, multithread=True, **kwargs):
        '''
        Flushes the internal write buffer.
        '''
        if self._write_buf.tell() > 0:
            data = self._write_buf.getvalue()
            self._write_buf = StringIO.StringIO()

            if multithread:
                self._async_upload_part_request(data, index=self._cur_part, **kwargs)
            else:
                self.upload_part(data, self._cur_part, **kwargs)

            self._cur_part += 1

        if len(self._http_threadpool_futures) > 0:
            dxpy.utils.wait_for_all_futures(self._http_threadpool_futures)
            for future in self._http_threadpool_futures:
                if future.exception() != None:
                    raise future.exception()
            self._http_threadpool_futures = set()

    def _async_upload_part_request(self, *args, **kwargs):
        if self._http_threadpool == None:
            DXFile._http_threadpool = dxpy.utils.get_futures_threadpool(max_workers=self._http_threadpool_size)

        while len(self._http_threadpool_futures) >= self._http_threadpool_size:
            future = dxpy.utils.wait_for_a_future(self._http_threadpool_futures)
            if future.exception() != None:
                raise future.exception()
            self._http_threadpool_futures.remove(future)

        future = self._http_threadpool.submit(self.upload_part, *args, **kwargs)
        self._http_threadpool_futures.add(future)

    def write(self, data, multithread=True, **kwargs):
        '''
        :param data: Data to be written
        :type data: str or mmap object

        Writes the data *data* to the file.

        .. note::

            Writing to remote files is append-only. Using :meth:`seek`
            does not affect where the next :meth:`write` will occur.

        '''

        def write_request(data):
            if multithread:
                self._async_upload_part_request(data, index=self._cur_part, **kwargs)
            else:
                self.upload_part(data, self._cur_part, **kwargs)
            self._cur_part += 1

        if self._write_buf.tell() == 0 and self._write_bufsize == len(data):
            # In the special case of a write that is the same size as
            # our write buffer size, and no unflushed data in the
            # buffer, just directly dispatch the write and bypass the
            # write buffer.
            #
            # This saves a buffer copy, which is especially helpful if
            # 'data' is actually mmap'd from a file.
            #
            # TODO: an additional optimization could be made to allow
            # the last request from an mmap'd upload to take this path
            # too (in general it won't because it's not of length
            # _write_bufsize). This is probably inconsequential though.
            write_request(data)
            return

        remaining_space = self._write_bufsize - self._write_buf.tell()

        if len(data) <= remaining_space:
            self._write_buf.write(data)
        else:
            self._write_buf.write(data[:remaining_space])

            data = self._write_buf.getvalue()
            self._write_buf = StringIO.StringIO()
            write_request(data)

            # TODO: check if repeat string splitting is bad for
            # performance when len(data) >> _write_bufsize
            self.write(data[remaining_space:], **kwargs)

    def closed(self, **kwargs):
        '''
        :returns: Whether the remote file is closed
        :rtype: boolean

        Returns :const:`True` if the remote file is closed and
        :const:`False` otherwise. Note that if it is not closed, it can
        be in either the "open" or "closing" states.
        '''

        return self.describe(**kwargs)["state"] == "closed"

    def close(self, block=False, **kwargs):
        '''
        :param block: If True, this function blocks until the remote file has closed.
        :type block: boolean

        Attempts to close the file.

        .. note:: The remote file cannot be closed until all parts have
           been fully uploaded. An exception will be thrown if this is
           not the case.
        '''
        self.flush(**kwargs)

        if self._num_uploaded_parts == 0: # File is empty, upload an empty part (files with 0 parts cannot be closed)
            self.upload_part('', 1, **kwargs)

        if 'report_progress_fn' in kwargs:
            del kwargs['report_progress_fn']

        dxpy.api.fileClose(self._dxid, **kwargs)

        if block:
            self._wait_on_close(**kwargs)

    def wait_on_close(self, timeout=sys.maxint, **kwargs):
        '''
        :param timeout: Maximum amount of time to wait (in seconds) until the file is closed.
        :type timeout: integer
        :raises: :exc:`dxpy.exceptions.DXFileError` if the timeout is reached before the remote file has been closed

        Waits until the remote file is closed.
        '''
        self._wait_on_close(timeout, **kwargs)

    def upload_part(self, data, index=None, display_progress=False, report_progress_fn=None, **kwargs):
        """
        :param data: Data to be uploaded in this part
        :type data: str or mmap object
        :param index: Index of part to be uploaded; must be in [1, 10000]
        :type index: integer
        :param display_progress: Whether to print "." to stderr when done
        :type display_progress: boolean
        :param report_progress_fn: Optional: a function to call that takes in two arguments (self, # bytes transmitted)
        :type report_progress_fn: function or None
        :raises: :exc:`dxpy.exceptions.DXFileError` if *index* is given and is not in the correct range, :exc:`requests.exceptions.HTTPError` if upload fails

        Uploads the data in *data* as part number *index* for the
        associated file. If no value for *index* is given, *index*
        defaults to 1. This probably only makes sense if this is the
        only part to be uploaded.
        """

        req_input = {}
        if index is not None:
            req_input["index"] = int(index)

        resp = dxpy.api.fileUpload(self._dxid, req_input, **kwargs)
        url = resp["url"]
        headers = {}
        headers['Content-Length'] = str(len(data))
        headers['Content-Type'] = 'application/octet-stream'

        DXHTTPRequest(url, data, headers=headers, jsonify_data=False, prepend_srv=False, always_retry=True)

        self._num_uploaded_parts += 1

        if display_progress:
            print >> sys.stderr, "."

        if report_progress_fn is not None:
            report_progress_fn(self, len(data))

    def get_download_url(self, duration=24*3600, **kwargs):
        if self._download_url is None or self._download_url_expires > time.time():
            # logging.debug("Download URL unset or expired, requesting a new one")
            resp = dxpy.api.fileDownload(self._dxid, {"duration": duration}, **kwargs)
            self._download_url = resp["url"]
            self._download_url_expires = time.time() + duration - 60 # Try to account for drift
        return self._download_url

    def _generate_read_requests(self, start_pos=0, end_pos=None, **kwargs):
        url = self.get_download_url(**kwargs)

        if self._file_length == None:
            desc = self.describe(**kwargs)
            self._file_length = int(desc["size"])

        if end_pos == None:
            end_pos = self._file_length
        if end_pos > self._file_length:
            raise DXFileError("Invalid end_pos")

        for chunk_start_pos in xrange(start_pos, end_pos, self._read_bufsize):
            chunk_end_pos = min(chunk_start_pos + self._read_bufsize - 1, end_pos)
            headers = {'Range': "bytes=" + str(chunk_start_pos) + "-" + str(chunk_end_pos)}
            yield DXHTTPRequest, [url, ''], {'method': 'GET',
                                             'headers': headers,
                                             'jsonify_data': False,
                                             'prepend_srv': False,
                                             'prefetch': True,
                                             'always_retry': True}

    def _next_response_content(self):
        if self._http_threadpool is None:
            DXFile._http_threadpool = dxpy.utils.get_futures_threadpool(max_workers=self._http_threadpool_size)

        if self._response_iterator is None:
            self._response_iterator = dxpy.utils.response_iterator(self._request_iterator, self._http_threadpool,
                                                                   max_active_tasks=self._http_threadpool_size)
        return self._response_iterator.next()

    def read(self, length=None, use_compression=None, **kwargs):
        '''
        :param size: Maximum number of bytes to be read
        :type size: integer
        :rtype: string

        Returns the next *size* bytes, or all the bytes until the end of
        file (if no *size* is given or there are fewer than *size* bytes
        left in the file).

        .. note:: After the first call to read(), passthrough kwargs are
           not respected while using the same response iterator (i.e.
           until next seek).

        '''
        if self._response_iterator == None:
            self._request_iterator = self._generate_read_requests(start_pos=self._pos, **kwargs)

        if self._file_length == None:
            desc = self.describe(**kwargs)
            if desc["state"] != "closed":
                raise DXFileError("Cannot read from file until it is in the closed state")
            self._file_length = int(desc["size"])

        if self._pos == self._file_length:
            return ""

        if length == None or length > self._file_length - self._pos:
            length = self._file_length - self._pos

        buf = self._read_buf
        buf_remaining_bytes = dxpy.utils.string_buffer_length(buf) - buf.tell()
        if length <= buf_remaining_bytes:
            self._pos += length
            return buf.read(length)
        else:
            orig_buf_pos = buf.tell()
            orig_file_pos = self._pos
            buf.seek(0, os.SEEK_END)
            self._pos += buf_remaining_bytes
            while self._pos < orig_file_pos + length:
                remaining_len = orig_file_pos + length - self._pos
                content = self._next_response_content()

                if len(content) < remaining_len:
                    buf.write(content)
                    self._pos += len(content)
                else: # response goes beyond requested length
                    buf.write(content[:remaining_len])
                    self._pos += remaining_len
                    self._read_buf = StringIO.StringIO()
                    self._read_buf.write(content[remaining_len:])
                    self._read_buf.seek(0)
            buf.seek(orig_buf_pos)
            return buf.read()

        # Debug fallback
        # import urllib2
        # req = urllib2.Request(url, headers=headers)
        # response = urllib2.urlopen(req)
        # return response.read()
