# mimetypes.el -- an elisp library for guessing mimetypes

Uses platform-dependent configurations for guessing the MIME type of a
file extension.

## Windows

The Windows registry is queried. `HKEY_LOCAL_MACHINE\Software\Classes`
is checked for an entry for the extension.  If the `Content Type` value under the
extension's registry entry exists that is returned.

## *nix

Guessing is done with a list of known mime.types files.  See variable
`mimetypes-known-files`.  The first file found is searched for the
desired extension.  If one is not found, the remaining files are not
checked for existence or searched.

## MS-Dos / Cygwin

Determination of a mechanism, implementation and testing is still
TODO.  If needed please open a ticket.

## extra-types argument

The optional argument `extra-types` can be used to specify a list of
additional MIME types.  Each element should be a list of strings.  The
first element being the MIME type, the remaining items being the
mapped file extensions.

## User .mime.types file

If a user-provided .mime.types file is found, the value provided there
takes precendence over the platform-determined mechanism and
extra-types list.  If a MIME type isn't found for the extension in
this file the platform-determined mechanism is used.


For Windows, this file is searched for under `%USERPROFILE\.mime.types`.

For all other platforms, `$HOME/.mime.types` is searched.

## Examples

### Guess mimetype for a file:

Guess a MIME type TARGET, which can be a buffer or string.

For buffers, an attempt will be made to get the MIME type with
the `file' command if it is available.  If the buffer has a
visited file the file name will specified as an argument to the
`file' command, otherwise the buffer's contents will be passed
through standard input.  If this check returns 'nil' or
'text/plain' a check is made for the MIME type by extension.

If TARGET is a string it is assumed to either be a file name or
an extension.  If a file by the name TARGET exists, a check is
made with the `file' command.  If this returns nil or
'text/plain' then check is made with the extension derived from
TARGET or TARGET itself.

If EXTRA-TYPES is provided, it takes priority over other guessing
mechanism.  If the extension derived from the buffer or string is
found in EXTRA-TYPES it will be returned.

```
(mimetypes-guess-mime "/path/to/foo.json")
-> "application/json"
```

```
(mimetypes-guess-mime (get-buffer "foo.json"))
-> "application/json"
```

To skip checking with the `file` process, set the variable
`mimetypes-bypass-file-proc` to non-nil:

```
(let ((mimetypes-bypass-file-proc t))
  (mimetypes-guess-mime "jpg"))
-> "application/json"
```
