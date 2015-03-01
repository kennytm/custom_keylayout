// Temporary module to represent stdin/stdout before RFC #899 is merged.

use std::io::{Read, BufRead, Result, Error, ErrorKind};
use std::old_io::stdio::{StdinReader, StdinReaderGuard};
use std::old_io::stdio::stdin as old_stdin;
use std::old_io::{IoResult, IoErrorKind, Reader, Buffer};

pub struct StdinLock<'a>(StdinReaderGuard<'a>);

pub struct Stdin(StdinReader);

pub fn stdin() -> Stdin {
    Stdin(old_stdin())
}

impl Stdin {
    pub fn lock<'a>(&'a mut self) -> StdinLock<'a> {
        StdinLock(self.0.lock())
    }
}


fn to_new_io_result<T>(res: IoResult<T>, eof_default: T) -> Result<T> {
    match res {
        Ok(x) => Ok(x),
        Err(e) => {
            let new_io_error_kind = match e.kind {
                IoErrorKind::EndOfFile => return Ok(eof_default),
                IoErrorKind::FileNotFound => ErrorKind::FileNotFound,
                IoErrorKind::PermissionDenied => ErrorKind::PermissionDenied,
                IoErrorKind::ConnectionRefused => ErrorKind::ConnectionRefused,
                IoErrorKind::ConnectionReset => ErrorKind::ConnectionReset,
                IoErrorKind::ConnectionAborted => ErrorKind::ConnectionAborted,
                IoErrorKind::NotConnected => ErrorKind::NotConnected,
                IoErrorKind::BrokenPipe => ErrorKind::BrokenPipe,
                IoErrorKind::PathAlreadyExists => ErrorKind::PathAlreadyExists,
                IoErrorKind::PathDoesntExist => ErrorKind::PathDoesntExist,
                IoErrorKind::MismatchedFileTypeForOperation => ErrorKind::MismatchedFileTypeForOperation,
                IoErrorKind::ResourceUnavailable => ErrorKind::ResourceUnavailable,
                IoErrorKind::InvalidInput => ErrorKind::InvalidInput,
                IoErrorKind::TimedOut => ErrorKind::TimedOut,
                e => panic!("unrecognized old I/O error kind: {:?}", e),
            };
            Err(Error::new(new_io_error_kind, e.desc, e.detail))
        }
    }
}

impl Read for Stdin {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        to_new_io_result(self.0.read(buf), 0)
    }
}

impl<'a> Read for StdinLock<'a> {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        to_new_io_result(self.0.read(buf), 0)
    }
}


impl<'a> BufRead for StdinLock<'a> {
    fn fill_buf(&mut self) -> Result<&[u8]> {
        to_new_io_result(self.0.fill_buf(), b"")
    }

    fn consume(&mut self, amount: usize) {
        self.0.consume(amount);
    }
}

