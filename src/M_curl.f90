! M_curl.f90
!
! Fortran 2008 ISO C binding interfaces to libcurl.
!
! Author:  Philipp Engel
! Licence: ISC
module M_curl
    use, intrinsic :: iso_c_binding, only: c_associated, c_char, c_f_pointer, c_funptr, &
                                           c_int, c_int64_t, c_loc, c_long, c_ptr, c_size_t
    implicit none

    public :: c_f_str_ptr

    public :: curl_easy_init
    public :: curl_easy_perform
    public :: curl_easy_cleanup
    public :: curl_easy_setopt
    public :: curl_easy_setopt_
    public :: curl_slist_append
    public :: curl_slist_free_all
    public :: curl_version_info
    public :: curl_version_now

    private :: curl_easy_setopt_char
    private :: curl_easy_setopt_fptr
    private :: curl_easy_setopt_int
    private :: curl_easy_setopt_long
    private :: curl_easy_setopt_ptr

    integer(kind=c_int), parameter :: CURLOPTTYPE_LONG          = 0
    integer(kind=c_int), parameter :: CURLOPTTYPE_OBJECTPOINT   = 10000
    integer(kind=c_int), parameter :: CURLOPTTYPE_STRINGPOINT   = 10000
    integer(kind=c_int), parameter :: CURLOPTTYPE_FUNCTIONPOINT = 20000
    integer(kind=c_int), parameter :: CURLOPTTYPE_OFF_T         = 30000

    integer(kind=c_int), parameter :: CURLOPT_WRITEDATA                  = CURLOPTTYPE_OBJECTPOINT + 1
    integer(kind=c_int), parameter :: CURLOPT_URL                        = CURLOPTTYPE_OBJECTPOINT + 2
    integer(kind=c_int), parameter :: CURLOPT_PORT                       = CURLOPTTYPE_LONG + 3
    integer(kind=c_int), parameter :: CURLOPT_PROXY                      = CURLOPTTYPE_OBJECTPOINT + 4
    integer(kind=c_int), parameter :: CURLOPT_USERPWD                    = CURLOPTTYPE_OBJECTPOINT + 5
    integer(kind=c_int), parameter :: CURLOPT_PROXYUSERPWD               = CURLOPTTYPE_OBJECTPOINT + 6
    integer(kind=c_int), parameter :: CURLOPT_RANGE                      = CURLOPTTYPE_OBJECTPOINT + 7
    integer(kind=c_int), parameter :: CURLOPT_READDATA                   = CURLOPTTYPE_OBJECTPOINT + 9
    integer(kind=c_int), parameter :: CURLOPT_ERRORBUFFER                = CURLOPTTYPE_OBJECTPOINT + 10
    integer(kind=c_int), parameter :: CURLOPT_WRITEFUNCTION              = CURLOPTTYPE_FUNCTIONPOINT + 11
    integer(kind=c_int), parameter :: CURLOPT_READFUNCTION               = CURLOPTTYPE_FUNCTIONPOINT + 12
    integer(kind=c_int), parameter :: CURLOPT_TIMEOUT                    = CURLOPTTYPE_LONG + 13
    integer(kind=c_int), parameter :: CURLOPT_INFILESIZE                 = CURLOPTTYPE_LONG + 14
    integer(kind=c_int), parameter :: CURLOPT_POSTFIELDS                 = CURLOPTTYPE_OBJECTPOINT + 15
    integer(kind=c_int), parameter :: CURLOPT_REFERER                    = CURLOPTTYPE_OBJECTPOINT + 16
    integer(kind=c_int), parameter :: CURLOPT_FTPPORT                    = CURLOPTTYPE_OBJECTPOINT + 17
    integer(kind=c_int), parameter :: CURLOPT_USERAGENT                  = CURLOPTTYPE_OBJECTPOINT + 18
    integer(kind=c_int), parameter :: CURLOPT_LOW_SPEED_LIMIT            = CURLOPTTYPE_LONG + 19
    integer(kind=c_int), parameter :: CURLOPT_LOW_SPEED_TIME             = CURLOPTTYPE_LONG + 20
    integer(kind=c_int), parameter :: CURLOPT_RESUME_FROM                = CURLOPTTYPE_LONG + 21
    integer(kind=c_int), parameter :: CURLOPT_COOKIE                     = CURLOPTTYPE_OBJECTPOINT + 22
    integer(kind=c_int), parameter :: CURLOPT_HTTPHEADER                 = CURLOPTTYPE_OBJECTPOINT + 23
    integer(kind=c_int), parameter :: CURLOPT_HTTPPOST                   = CURLOPTTYPE_OBJECTPOINT + 24
    integer(kind=c_int), parameter :: CURLOPT_SSLCERT                    = CURLOPTTYPE_OBJECTPOINT + 25
    integer(kind=c_int), parameter :: CURLOPT_KEYPASSWD                  = CURLOPTTYPE_OBJECTPOINT + 26
    integer(kind=c_int), parameter :: CURLOPT_CRLF                       = CURLOPTTYPE_LONG + 27
    integer(kind=c_int), parameter :: CURLOPT_QUOTE                      = CURLOPTTYPE_OBJECTPOINT + 28
    integer(kind=c_int), parameter :: CURLOPT_HEADERDATA                 = CURLOPTTYPE_OBJECTPOINT + 29
    integer(kind=c_int), parameter :: CURLOPT_COOKIEFILE                 = CURLOPTTYPE_OBJECTPOINT + 31
    integer(kind=c_int), parameter :: CURLOPT_SSLVERSION                 = CURLOPTTYPE_LONG + 32
    integer(kind=c_int), parameter :: CURLOPT_TIMECONDITION              = CURLOPTTYPE_LONG + 33
    integer(kind=c_int), parameter :: CURLOPT_TIMEVALUE                  = CURLOPTTYPE_LONG + 34
    integer(kind=c_int), parameter :: CURLOPT_CUSTOMREQUEST              = CURLOPTTYPE_OBJECTPOINT + 36
    integer(kind=c_int), parameter :: CURLOPT_STDERR                     = CURLOPTTYPE_OBJECTPOINT + 37
    integer(kind=c_int), parameter :: CURLOPT_POSTQUOTE                  = CURLOPTTYPE_OBJECTPOINT + 39
    integer(kind=c_int), parameter :: CURLOPT_OBSOLETE40                 = CURLOPTTYPE_OBJECTPOINT + 40
    integer(kind=c_int), parameter :: CURLOPT_VERBOSE                    = CURLOPTTYPE_LONG + 41
    integer(kind=c_int), parameter :: CURLOPT_HEADER                     = CURLOPTTYPE_LONG + 42
    integer(kind=c_int), parameter :: CURLOPT_NOPROGRESS                 = CURLOPTTYPE_LONG + 43
    integer(kind=c_int), parameter :: CURLOPT_NOBODY                     = CURLOPTTYPE_LONG + 44
    integer(kind=c_int), parameter :: CURLOPT_FAILONERROR                = CURLOPTTYPE_LONG + 45
    integer(kind=c_int), parameter :: CURLOPT_UPLOAD                     = CURLOPTTYPE_LONG + 46
    integer(kind=c_int), parameter :: CURLOPT_POST                       = CURLOPTTYPE_LONG + 47
    integer(kind=c_int), parameter :: CURLOPT_DIRLISTONLY                = CURLOPTTYPE_LONG + 48
    integer(kind=c_int), parameter :: CURLOPT_APPEND                     = CURLOPTTYPE_LONG + 50
    integer(kind=c_int), parameter :: CURLOPT_NETRC                      = CURLOPTTYPE_LONG + 51
    integer(kind=c_int), parameter :: CURLOPT_FOLLOWLOCATION             = CURLOPTTYPE_LONG + 52
    integer(kind=c_int), parameter :: CURLOPT_TRANSFERTEXT               = CURLOPTTYPE_LONG + 53
    integer(kind=c_int), parameter :: CURLOPT_PUT                        = CURLOPTTYPE_LONG + 54
    integer(kind=c_int), parameter :: CURLOPT_PROGRESSFUNCTION           = CURLOPTTYPE_FUNCTIONPOINT + 56
    integer(kind=c_int), parameter :: CURLOPT_PROGRESSDATA               = CURLOPTTYPE_OBJECTPOINT + 57
    integer(kind=c_int), parameter :: CURLOPT_AUTOREFERER                = CURLOPTTYPE_LONG + 58
    integer(kind=c_int), parameter :: CURLOPT_PROXYPORT                  = CURLOPTTYPE_LONG + 59
    integer(kind=c_int), parameter :: CURLOPT_POSTFIELDSIZE              = CURLOPTTYPE_LONG + 60
    integer(kind=c_int), parameter :: CURLOPT_HTTPPROXYTUNNEL            = CURLOPTTYPE_LONG + 61
    integer(kind=c_int), parameter :: CURLOPT_INTERFACE                  = CURLOPTTYPE_OBJECTPOINT + 62
    integer(kind=c_int), parameter :: CURLOPT_KRBLEVEL                   = CURLOPTTYPE_OBJECTPOINT + 63
    integer(kind=c_int), parameter :: CURLOPT_SSL_VERIFYPEER             = CURLOPTTYPE_LONG + 64
    integer(kind=c_int), parameter :: CURLOPT_CAINFO                     = CURLOPTTYPE_OBJECTPOINT + 65
    integer(kind=c_int), parameter :: CURLOPT_MAXREDIRS                  = CURLOPTTYPE_LONG + 68
    integer(kind=c_int), parameter :: CURLOPT_FILETIME                   = CURLOPTTYPE_LONG + 69
    integer(kind=c_int), parameter :: CURLOPT_TELNETOPTIONS              = CURLOPTTYPE_OBJECTPOINT + 70
    integer(kind=c_int), parameter :: CURLOPT_MAXCONNECTS                = CURLOPTTYPE_LONG + 71
    integer(kind=c_int), parameter :: CURLOPT_OBSOLETE72                 = CURLOPTTYPE_LONG + 72
    integer(kind=c_int), parameter :: CURLOPT_FRESH_CONNECT              = CURLOPTTYPE_LONG + 74
    integer(kind=c_int), parameter :: CURLOPT_FORBID_REUSE               = CURLOPTTYPE_LONG + 75
    integer(kind=c_int), parameter :: CURLOPT_RANDOM_FILE                = CURLOPTTYPE_OBJECTPOINT + 76
    integer(kind=c_int), parameter :: CURLOPT_EGDSOCKET                  = CURLOPTTYPE_OBJECTPOINT + 77
    integer(kind=c_int), parameter :: CURLOPT_CONNECTTIMEOUT             = CURLOPTTYPE_LONG + 78
    integer(kind=c_int), parameter :: CURLOPT_HEADERFUNCTION             = CURLOPTTYPE_FUNCTIONPOINT + 79
    integer(kind=c_int), parameter :: CURLOPT_HTTPGET                    = CURLOPTTYPE_LONG + 80
    integer(kind=c_int), parameter :: CURLOPT_SSL_VERIFYHOST             = CURLOPTTYPE_LONG + 81
    integer(kind=c_int), parameter :: CURLOPT_COOKIEJAR                  = CURLOPTTYPE_OBJECTPOINT + 82
    integer(kind=c_int), parameter :: CURLOPT_SSL_CIPHER_LIST            = CURLOPTTYPE_OBJECTPOINT + 83
    integer(kind=c_int), parameter :: CURLOPT_HTTP_VERSION               = CURLOPTTYPE_LONG + 84
    integer(kind=c_int), parameter :: CURLOPT_FTP_USE_EPSV               = CURLOPTTYPE_LONG + 85
    integer(kind=c_int), parameter :: CURLOPT_SSLCERTTYPE                = CURLOPTTYPE_OBJECTPOINT + 86
    integer(kind=c_int), parameter :: CURLOPT_SSLKEY                     = CURLOPTTYPE_OBJECTPOINT + 87
    integer(kind=c_int), parameter :: CURLOPT_SSLKEYTYPE                 = CURLOPTTYPE_OBJECTPOINT + 88
    integer(kind=c_int), parameter :: CURLOPT_SSLENGINE                  = CURLOPTTYPE_OBJECTPOINT + 89
    integer(kind=c_int), parameter :: CURLOPT_SSLENGINE_DEFAULT          = CURLOPTTYPE_LONG + 90
    integer(kind=c_int), parameter :: CURLOPT_DNS_USE_GLOBAL_CACHE       = CURLOPTTYPE_LONG + 91
    integer(kind=c_int), parameter :: CURLOPT_DNS_CACHE_TIMEOUT          = CURLOPTTYPE_LONG + 92
    integer(kind=c_int), parameter :: CURLOPT_PREQUOTE                   = CURLOPTTYPE_OBJECTPOINT + 93
    integer(kind=c_int), parameter :: CURLOPT_DEBUGFUNCTION              = CURLOPTTYPE_FUNCTIONPOINT + 94
    integer(kind=c_int), parameter :: CURLOPT_DEBUGDATA                  = CURLOPTTYPE_OBJECTPOINT + 95
    integer(kind=c_int), parameter :: CURLOPT_COOKIESESSION              = CURLOPTTYPE_LONG + 96
    integer(kind=c_int), parameter :: CURLOPT_CAPATH                     = CURLOPTTYPE_OBJECTPOINT + 97
    integer(kind=c_int), parameter :: CURLOPT_BUFFERSIZE                 = CURLOPTTYPE_LONG + 98
    integer(kind=c_int), parameter :: CURLOPT_NOSIGNAL                   = CURLOPTTYPE_LONG + 99
    integer(kind=c_int), parameter :: CURLOPT_SHARE                      = CURLOPTTYPE_OBJECTPOINT + 100
    integer(kind=c_int), parameter :: CURLOPT_PROXYTYPE                  = CURLOPTTYPE_LONG + 101
    integer(kind=c_int), parameter :: CURLOPT_ACCEPT_ENCODING            = CURLOPTTYPE_OBJECTPOINT + 102
    integer(kind=c_int), parameter :: CURLOPT_PRIVATE                    = CURLOPTTYPE_OBJECTPOINT + 103
    integer(kind=c_int), parameter :: CURLOPT_HTTP200ALIASES             = CURLOPTTYPE_OBJECTPOINT + 104
    integer(kind=c_int), parameter :: CURLOPT_UNRESTRICTED_AUTH          = CURLOPTTYPE_LONG + 105
    integer(kind=c_int), parameter :: CURLOPT_FTP_USE_EPRT               = CURLOPTTYPE_LONG + 106
    integer(kind=c_int), parameter :: CURLOPT_HTTPAUTH                   = CURLOPTTYPE_LONG + 107
    integer(kind=c_int), parameter :: CURLOPT_SSL_CTX_FUNCTION           = CURLOPTTYPE_FUNCTIONPOINT + 108
    integer(kind=c_int), parameter :: CURLOPT_SSL_CTX_DATA               = CURLOPTTYPE_OBJECTPOINT + 109
    integer(kind=c_int), parameter :: CURLOPT_FTP_CREATE_MISSING_DIRS    = CURLOPTTYPE_LONG + 110
    integer(kind=c_int), parameter :: CURLOPT_PROXYAUTH                  = CURLOPTTYPE_LONG + 111
    integer(kind=c_int), parameter :: CURLOPT_FTP_RESPONSE_TIMEOUT       = CURLOPTTYPE_LONG + 112
    integer(kind=c_int), parameter :: CURLOPT_IPRESOLVE                  = CURLOPTTYPE_LONG + 113
    integer(kind=c_int), parameter :: CURLOPT_MAXFILESIZE                = CURLOPTTYPE_LONG + 114
    integer(kind=c_int), parameter :: CURLOPT_INFILESIZE_LARGE           = CURLOPTTYPE_OFF_T + 115
    integer(kind=c_int), parameter :: CURLOPT_RESUME_FROM_LARGE          = CURLOPTTYPE_OFF_T + 116
    integer(kind=c_int), parameter :: CURLOPT_MAXFILESIZE_LARGE          = CURLOPTTYPE_OFF_T + 117
    integer(kind=c_int), parameter :: CURLOPT_NETRC_FILE                 = CURLOPTTYPE_OBJECTPOINT + 118
    integer(kind=c_int), parameter :: CURLOPT_USE_SSL                    = CURLOPTTYPE_LONG + 119
    integer(kind=c_int), parameter :: CURLOPT_POSTFIELDSIZE_LARGE        = CURLOPTTYPE_OFF_T + 120
    integer(kind=c_int), parameter :: CURLOPT_TCP_NODELAY                = CURLOPTTYPE_LONG + 121
    integer(kind=c_int), parameter :: CURLOPT_FTPSSLAUTH                 = CURLOPTTYPE_LONG + 129
    integer(kind=c_int), parameter :: CURLOPT_IOCTLFUNCTION              = CURLOPTTYPE_FUNCTIONPOINT + 130
    integer(kind=c_int), parameter :: CURLOPT_IOCTLDATA                  = CURLOPTTYPE_OBJECTPOINT + 131
    integer(kind=c_int), parameter :: CURLOPT_FTP_ACCOUNT                = CURLOPTTYPE_OBJECTPOINT + 134
    integer(kind=c_int), parameter :: CURLOPT_COOKIELIST                 = CURLOPTTYPE_OBJECTPOINT + 135
    integer(kind=c_int), parameter :: CURLOPT_IGNORE_CONTENT_LENGTH      = CURLOPTTYPE_LONG + 136
    integer(kind=c_int), parameter :: CURLOPT_FTP_SKIP_PASV_IP           = CURLOPTTYPE_LONG + 137
    integer(kind=c_int), parameter :: CURLOPT_FTP_FILEMETHOD             = CURLOPTTYPE_LONG + 138
    integer(kind=c_int), parameter :: CURLOPT_LOCALPORT                  = CURLOPTTYPE_LONG + 139
    integer(kind=c_int), parameter :: CURLOPT_LOCALPORTRANGE             = CURLOPTTYPE_LONG + 140
    integer(kind=c_int), parameter :: CURLOPT_CONNECT_ONLY               = CURLOPTTYPE_LONG + 141
    integer(kind=c_int), parameter :: CURLOPT_CONV_FROM_NETWORK_FUNCTION = CURLOPTTYPE_FUNCTIONPOINT + 142
    integer(kind=c_int), parameter :: CURLOPT_CONV_TO_NETWORK_FUNCTION   = CURLOPTTYPE_FUNCTIONPOINT + 143
    integer(kind=c_int), parameter :: CURLOPT_CONV_FROM_UTF8_FUNCTION    = CURLOPTTYPE_FUNCTIONPOINT + 144
    integer(kind=c_int), parameter :: CURLOPT_MAX_SEND_SPEED_LARGE       = CURLOPTTYPE_OFF_T + 145
    integer(kind=c_int), parameter :: CURLOPT_MAX_RECV_SPEED_LARGE       = CURLOPTTYPE_OFF_T + 146
    integer(kind=c_int), parameter :: CURLOPT_FTP_ALTERNATIVE_TO_USER    = CURLOPTTYPE_OBJECTPOINT + 147
    integer(kind=c_int), parameter :: CURLOPT_SOCKOPTFUNCTION            = CURLOPTTYPE_FUNCTIONPOINT + 148
    integer(kind=c_int), parameter :: CURLOPT_SOCKOPTDATA                = CURLOPTTYPE_OBJECTPOINT + 149
    integer(kind=c_int), parameter :: CURLOPT_SSL_SESSIONID_CACHE        = CURLOPTTYPE_LONG + 150
    integer(kind=c_int), parameter :: CURLOPT_SSH_AUTH_TYPES             = CURLOPTTYPE_LONG + 151
    integer(kind=c_int), parameter :: CURLOPT_SSH_PUBLIC_KEYFILE         = CURLOPTTYPE_OBJECTPOINT + 152
    integer(kind=c_int), parameter :: CURLOPT_SSH_PRIVATE_KEYFILE        = CURLOPTTYPE_OBJECTPOINT + 153
    integer(kind=c_int), parameter :: CURLOPT_FTP_SSL_CCC                = CURLOPTTYPE_LONG + 154
    integer(kind=c_int), parameter :: CURLOPT_TIMEOUT_MS                 = CURLOPTTYPE_LONG + 155
    integer(kind=c_int), parameter :: CURLOPT_CONNECTTIMEOUT_MS          = CURLOPTTYPE_LONG + 156
    integer(kind=c_int), parameter :: CURLOPT_HTTP_TRANSFER_DECODING     = CURLOPTTYPE_LONG + 157
    integer(kind=c_int), parameter :: CURLOPT_HTTP_CONTENT_DECODING      = CURLOPTTYPE_LONG + 158
    integer(kind=c_int), parameter :: CURLOPT_NEW_FILE_PERMS             = CURLOPTTYPE_LONG + 159
    integer(kind=c_int), parameter :: CURLOPT_NEW_DIRECTORY_PERMS        = CURLOPTTYPE_LONG + 160
    integer(kind=c_int), parameter :: CURLOPT_POSTREDIR                  = CURLOPTTYPE_LONG + 161
    integer(kind=c_int), parameter :: CURLOPT_SSH_HOST_PUBLIC_KEY_MD5    = CURLOPTTYPE_OBJECTPOINT + 162
    integer(kind=c_int), parameter :: CURLOPT_OPENSOCKETFUNCTION         = CURLOPTTYPE_FUNCTIONPOINT + 163
    integer(kind=c_int), parameter :: CURLOPT_OPENSOCKETDATA             = CURLOPTTYPE_OBJECTPOINT + 164
    integer(kind=c_int), parameter :: CURLOPT_COPYPOSTFIELDS             = CURLOPTTYPE_OBJECTPOINT + 165
    integer(kind=c_int), parameter :: CURLOPT_PROXY_TRANSFER_MODE        = CURLOPTTYPE_LONG + 166
    integer(kind=c_int), parameter :: CURLOPT_SEEKFUNCTION               = CURLOPTTYPE_FUNCTIONPOINT + 167
    integer(kind=c_int), parameter :: CURLOPT_SEEKDATA                   = CURLOPTTYPE_OBJECTPOINT + 168
    integer(kind=c_int), parameter :: CURLOPT_CRLFILE                    = CURLOPTTYPE_OBJECTPOINT + 169
    integer(kind=c_int), parameter :: CURLOPT_ISSUERCERT                 = CURLOPTTYPE_OBJECTPOINT + 170
    integer(kind=c_int), parameter :: CURLOPT_ADDRESS_SCOPE              = CURLOPTTYPE_LONG + 171
    integer(kind=c_int), parameter :: CURLOPT_CERTINFO                   = CURLOPTTYPE_LONG + 172
    integer(kind=c_int), parameter :: CURLOPT_USERNAME                   = CURLOPTTYPE_OBJECTPOINT + 173
    integer(kind=c_int), parameter :: CURLOPT_PASSWORD                   = CURLOPTTYPE_OBJECTPOINT + 174
    integer(kind=c_int), parameter :: CURLOPT_PROXYUSERNAME              = CURLOPTTYPE_OBJECTPOINT + 175
    integer(kind=c_int), parameter :: CURLOPT_PROXYPASSWORD              = CURLOPTTYPE_OBJECTPOINT + 176
    integer(kind=c_int), parameter :: CURLOPT_NOPROXY                    = CURLOPTTYPE_OBJECTPOINT + 177
    integer(kind=c_int), parameter :: CURLOPT_TFTP_BLKSIZE               = CURLOPTTYPE_LONG + 178
    integer(kind=c_int), parameter :: CURLOPT_SOCKS5_GSSAPI_SERVICE      = CURLOPTTYPE_OBJECTPOINT + 179
    integer(kind=c_int), parameter :: CURLOPT_SOCKS5_GSSAPI_NEC          = CURLOPTTYPE_LONG + 180
    integer(kind=c_int), parameter :: CURLOPT_PROTOCOLS                  = CURLOPTTYPE_LONG + 181
    integer(kind=c_int), parameter :: CURLOPT_REDIR_PROTOCOLS            = CURLOPTTYPE_LONG + 182
    integer(kind=c_int), parameter :: CURLOPT_SSH_KNOWNHOSTS             = CURLOPTTYPE_OBJECTPOINT + 183
    integer(kind=c_int), parameter :: CURLOPT_SSH_KEYFUNCTION            = CURLOPTTYPE_FUNCTIONPOINT + 184
    integer(kind=c_int), parameter :: CURLOPT_SSH_KEYDATA                = CURLOPTTYPE_OBJECTPOINT + 185
    integer(kind=c_int), parameter :: CURLOPT_MAIL_FROM                  = CURLOPTTYPE_OBJECTPOINT + 186
    integer(kind=c_int), parameter :: CURLOPT_MAIL_RCPT                  = CURLOPTTYPE_OBJECTPOINT + 187
    integer(kind=c_int), parameter :: CURLOPT_FTP_USE_PRET               = CURLOPTTYPE_LONG + 188
    integer(kind=c_int), parameter :: CURLOPT_RTSP_REQUEST               = CURLOPTTYPE_LONG + 189
    integer(kind=c_int), parameter :: CURLOPT_RTSP_SESSION_ID            = CURLOPTTYPE_OBJECTPOINT + 190
    integer(kind=c_int), parameter :: CURLOPT_RTSP_STREAM_URI            = CURLOPTTYPE_OBJECTPOINT + 191
    integer(kind=c_int), parameter :: CURLOPT_RTSP_TRANSPORT             = CURLOPTTYPE_OBJECTPOINT + 192
    integer(kind=c_int), parameter :: CURLOPT_RTSP_CLIENT_CSEQ           = CURLOPTTYPE_LONG + 193
    integer(kind=c_int), parameter :: CURLOPT_RTSP_SERVER_CSEQ           = CURLOPTTYPE_LONG + 194
    integer(kind=c_int), parameter :: CURLOPT_INTERLEAVEDATA             = CURLOPTTYPE_OBJECTPOINT + 195
    integer(kind=c_int), parameter :: CURLOPT_INTERLEAVEFUNCTION         = CURLOPTTYPE_FUNCTIONPOINT + 196
    integer(kind=c_int), parameter :: CURLOPT_WILDCARDMATCH              = CURLOPTTYPE_LONG + 197
    integer(kind=c_int), parameter :: CURLOPT_CHUNK_BGN_FUNCTION         = CURLOPTTYPE_FUNCTIONPOINT + 198
    integer(kind=c_int), parameter :: CURLOPT_CHUNK_END_FUNCTION         = CURLOPTTYPE_FUNCTIONPOINT + 199
    integer(kind=c_int), parameter :: CURLOPT_FNMATCH_FUNCTION           = CURLOPTTYPE_FUNCTIONPOINT + 200
    integer(kind=c_int), parameter :: CURLOPT_CHUNK_DATA                 = CURLOPTTYPE_OBJECTPOINT + 201
    integer(kind=c_int), parameter :: CURLOPT_FNMATCH_DATA               = CURLOPTTYPE_OBJECTPOINT + 202
    integer(kind=c_int), parameter :: CURLOPT_RESOLVE                    = CURLOPTTYPE_OBJECTPOINT + 203
    integer(kind=c_int), parameter :: CURLOPT_TLSAUTH_USERNAME           = CURLOPTTYPE_OBJECTPOINT + 204
    integer(kind=c_int), parameter :: CURLOPT_TLSAUTH_PASSWORD           = CURLOPTTYPE_OBJECTPOINT + 205
    integer(kind=c_int), parameter :: CURLOPT_TLSAUTH_TYPE               = CURLOPTTYPE_OBJECTPOINT + 206
    integer(kind=c_int), parameter :: CURLOPT_TRANSFER_ENCODING          = CURLOPTTYPE_LONG + 207
    integer(kind=c_int), parameter :: CURLOPT_CLOSESOCKETFUNCTION        = CURLOPTTYPE_FUNCTIONPOINT + 208
    integer(kind=c_int), parameter :: CURLOPT_CLOSESOCKETDATA            = CURLOPTTYPE_OBJECTPOINT + 209
    integer(kind=c_int), parameter :: CURLOPT_GSSAPI_DELEGATION          = CURLOPTTYPE_LONG + 210
    integer(kind=c_int), parameter :: CURLOPT_DNS_SERVERS                = CURLOPTTYPE_OBJECTPOINT + 211
    integer(kind=c_int), parameter :: CURLOPT_ACCEPTTIMEOUT_MS           = CURLOPTTYPE_LONG + 212
    integer(kind=c_int), parameter :: CURLOPT_TCP_KEEPALIVE              = CURLOPTTYPE_LONG + 213
    integer(kind=c_int), parameter :: CURLOPT_TCP_KEEPIDLE               = CURLOPTTYPE_LONG + 214
    integer(kind=c_int), parameter :: CURLOPT_TCP_KEEPINTVL              = CURLOPTTYPE_LONG + 215
    integer(kind=c_int), parameter :: CURLOPT_SSL_OPTIONS                = CURLOPTTYPE_LONG + 216
    integer(kind=c_int), parameter :: CURLOPT_MAIL_AUTH                  = CURLOPTTYPE_OBJECTPOINT + 217
    integer(kind=c_int), parameter :: CURLOPT_SASL_IR                    = CURLOPTTYPE_LONG + 218
    integer(kind=c_int), parameter :: CURLOPT_XFERINFOFUNCTION           = CURLOPTTYPE_FUNCTIONPOINT + 219
    integer(kind=c_int), parameter :: CURLOPT_XOAUTH2_BEARER             = CURLOPTTYPE_OBJECTPOINT + 220
    integer(kind=c_int), parameter :: CURLOPT_DNS_INTERFACE              = CURLOPTTYPE_OBJECTPOINT + 221
    integer(kind=c_int), parameter :: CURLOPT_DNS_LOCAL_IP4              = CURLOPTTYPE_OBJECTPOINT + 222
    integer(kind=c_int), parameter :: CURLOPT_DNS_LOCAL_IP6              = CURLOPTTYPE_OBJECTPOINT + 223
    integer(kind=c_int), parameter :: CURLOPT_LOGIN_OPTIONS              = CURLOPTTYPE_OBJECTPOINT + 224
    integer(kind=c_int), parameter :: CURLOPT_SSL_ENABLE_NPN             = CURLOPTTYPE_LONG + 225
    integer(kind=c_int), parameter :: CURLOPT_SSL_ENABLE_ALPN            = CURLOPTTYPE_LONG + 226
    integer(kind=c_int), parameter :: CURLOPT_EXPECT_100_TIMEOUT_MS      = CURLOPTTYPE_LONG + 227
    integer(kind=c_int), parameter :: CURLOPT_PROXYHEADER                = CURLOPTTYPE_OBJECTPOINT + 228
    integer(kind=c_int), parameter :: CURLOPT_HEADEROPT                  = CURLOPTTYPE_LONG + 229
    integer(kind=c_int), parameter :: CURLOPT_PINNEDPUBLICKEY            = CURLOPTTYPE_OBJECTPOINT + 230
    integer(kind=c_int), parameter :: CURLOPT_UNIX_SOCKET_PATH           = CURLOPTTYPE_OBJECTPOINT + 231
    integer(kind=c_int), parameter :: CURLOPT_DEFAULT_PROTOCOL           = CURLOPTTYPE_STRINGPOINT + 238

    integer(kind=c_int), parameter :: CURL_IPRESOLVE_WHATEVER = 0
    integer(kind=c_int), parameter :: CURL_IPRESOLVE_V4       = 1
    integer(kind=c_int), parameter :: CURL_IPRESOLVE_V6       = 2

    integer(kind=c_int), parameter :: CURL_GLOBAL_SSL       = shiftl(1, 0)
    integer(kind=c_int), parameter :: CURL_GLOBAL_WIN32     = shiftl(1, 1)
    integer(kind=c_int), parameter :: CURL_GLOBAL_ALL       = ior(CURL_GLOBAL_SSL, CURL_GLOBAL_WIN32)
    integer(kind=c_int), parameter :: CURL_GLOBAL_NOTHING   = 0
    integer(kind=c_int), parameter :: CURL_GLOBAL_DEFAULT   = CURL_GLOBAL_ALL
    integer(kind=c_int), parameter :: CURL_GLOBAL_ACK_EINTR = shiftl(1, 2)

    enum, bind(c)
        enumerator :: CURLE_OK
        enumerator :: CURLE_UNSUPPORTED_PROTOCOL
        enumerator :: CURLE_FAILED_INIT
        enumerator :: CURLE_URL_MALFORMAT
        enumerator :: CURLE_NOT_BUILT_IN
        enumerator :: CURLE_COULDNT_RESOLVE_PROXY
        enumerator :: CURLE_COULDNT_RESOLVE_HOST
        enumerator :: CURLE_COULDNT_CONNECT
        enumerator :: CURLE_WEIRD_SERVER_REPLY
        enumerator :: CURLE_REMOTE_ACCESS_DENIED
        enumerator :: CURLE_FTP_ACCEPT_FAILED
        enumerator :: CURLE_FTP_WEIRD_PASS_REPLY
        enumerator :: CURLE_FTP_ACCEPT_TIMEOUT
        enumerator :: CURLE_FTP_WEIRD_PASV_REPLY
        enumerator :: CURLE_FTP_WEIRD_227_FORMAT
        enumerator :: CURLE_FTP_CANT_GET_HOST
        enumerator :: CURLE_HTTP2
        enumerator :: CURLE_FTP_COULDNT_SET_TYPE
        enumerator :: CURLE_PARTIAL_FILE
        enumerator :: CURLE_FTP_COULDNT_RETR_FILE
        enumerator :: CURLE_OBSOLETE20
        enumerator :: CURLE_QUOTE_ERROR
        enumerator :: CURLE_HTTP_RETURNED_ERROR
        enumerator :: CURLE_WRITE_ERROR
        enumerator :: CURLE_OBSOLETE24
        enumerator :: CURLE_UPLOAD_FAILED
        enumerator :: CURLE_READ_ERROR
        enumerator :: CURLE_OUT_OF_MEMORY
        enumerator :: CURLE_OPERATION_TIMEDOUT
        enumerator :: CURLE_OBSOLETE29
        enumerator :: CURLE_FTP_PORT_FAILED
        enumerator :: CURLE_FTP_COULDNT_USE_REST
        enumerator :: CURLE_OBSOLETE32
        enumerator :: CURLE_RANGE_ERROR
        enumerator :: CURLE_HTTP_POST_ERROR
        enumerator :: CURLE_SSL_CONNECT_ERROR
        enumerator :: CURLE_BAD_DOWNLOAD_RESUME
        enumerator :: CURLE_FILE_COULDNT_READ_FILE
        enumerator :: CURLE_LDAP_CANNOT_BIND
        enumerator :: CURLE_LDAP_SEARCH_FAILED
        enumerator :: CURLE_OBSOLETE40
        enumerator :: CURLE_FUNCTION_NOT_FOUND
        enumerator :: CURLE_ABORTED_BY_CALLBACK
        enumerator :: CURLE_BAD_FUNCTION_ARGUMENT
        enumerator :: CURLE_OBSOLETE44
        enumerator :: CURLE_INTERFACE_FAILED
        enumerator :: CURLE_OBSOLETE46
        enumerator :: CURLE_TOO_MANY_REDIRECTS
        enumerator :: CURLE_UNKNOWN_OPTION
        enumerator :: CURLE_TELNET_OPTION_SYNTAX
        enumerator :: CURLE_OBSOLETE50
        enumerator :: CURLE_PEER_FAILED_VERIFICATION
        enumerator :: CURLE_GOT_NOTHING
        enumerator :: CURLE_SSL_ENGINE_NOTFOUND
        enumerator :: CURLE_SSL_ENGINE_SETFAILED
        enumerator :: CURLE_SEND_ERROR
        enumerator :: CURLE_RECV_ERROR
        enumerator :: CURLE_OBSOLETE57
        enumerator :: CURLE_SSL_CERTPROBLEM
        enumerator :: CURLE_SSL_CIPHER
        enumerator :: CURLE_SSL_CACERT
        enumerator :: CURLE_BAD_CONTENT_ENCODING
        enumerator :: CURLE_LDAP_INVALID_URL
        enumerator :: CURLE_FILESIZE_EXCEEDED
        enumerator :: CURLE_USE_SSL_FAILED
        enumerator :: CURLE_SEND_FAIL_REWIND
        enumerator :: CURLE_SSL_ENGINE_INITFAILED
        enumerator :: CURLE_LOGIN_DENIED
        enumerator :: CURLE_TFTP_NOTFOUND
        enumerator :: CURLE_TFTP_PERM
        enumerator :: CURLE_REMOTE_DISK_FULL
        enumerator :: CURLE_TFTP_ILLEGAL
        enumerator :: CURLE_TFTP_UNKNOWNID
        enumerator :: CURLE_REMOTE_FILE_EXISTS
        enumerator :: CURLE_TFTP_NOSUCHUSER
        enumerator :: CURLE_CONV_FAILED
        enumerator :: CURLE_CONV_REQD
        enumerator :: CURLE_SSL_CACERT_BADFILE
        enumerator :: CURLE_REMOTE_FILE_NOT_FOUND
        enumerator :: CURLE_SSH
        enumerator :: CURLE_SSL_SHUTDOWN_FAILED
        enumerator :: CURLE_AGAIN
        enumerator :: CURLE_SSL_CRL_BADFILE
        enumerator :: CURLE_SSL_ISSUER_ERROR
        enumerator :: CURLE_FTP_PRET_FAILED
        enumerator :: CURLE_RTSP_CSEQ_ERROR
        enumerator :: CURLE_RTSP_SESSION_ERROR
        enumerator :: CURLE_FTP_BAD_FILE_LIST
        enumerator :: CURLE_CHUNK_FAILED
        enumerator :: CURLE_NO_CONNECTION_AVAILABLE
        enumerator :: CURLE_SSL_PINNEDPUBKEYNOTMATCH
        enumerator :: CURLE_SSL_INVALIDCERTSTATUS
        enumerator :: CURLE_HTTP2_STREAM
        enumerator :: CURLE_RECURSIVE_API_CALL
        enumerator :: CURL_LAST
    end enum

    enum, bind(c)
        enumerator :: CURLVERSION_FIRST
        enumerator :: CURLVERSION_SECOND
        enumerator :: CURLVERSION_THIRD
        enumerator :: CURLVERSION_FOURTH
        enumerator :: CURLVERSION_FIFTH
        enumerator :: CURLVERSION_LAST
    end enum

    ! curl_version_info_data
    type, bind(c) :: curl_version
        integer(kind=c_int)  :: age
        type(c_ptr)          :: version
        integer(kind=c_int)  :: version_num
        type(c_ptr)          :: host
        integer(kind=c_int)  :: features
        type(c_ptr)          :: ssl_version
        integer(kind=c_long) :: ssl_version_num
        type(c_ptr)          :: libz_version
        type(c_ptr)          :: protocols
        type(c_ptr)          :: ares
        integer(kind=c_int)  :: ares_num
        type(c_ptr)          :: libidn
        integer(kind=c_int)  :: iconv_ver_num
        type(c_ptr)          :: libssh_version
        integer(kind=c_int)  :: brotli_ver_num
        type(c_ptr)          :: brotli_version
        integer(kind=c_int)  :: nghttp2_ver_num
        type(c_ptr)          :: nghttp2_version
        type(c_ptr)          :: quic_version
    end type curl_version

    interface
        ! CURL *curl_easy_init(void)
        function curl_easy_init() bind(c, name='curl_easy_init')
            import :: c_ptr
            type(c_ptr) :: curl_easy_init
        end function curl_easy_init

        ! CURLcode curl_easy_perform(CURL *curl)
        function curl_easy_perform(curl) bind(c, name='curl_easy_perform')
            import :: c_int, c_ptr
            type(c_ptr), intent(in), value :: curl
            integer(kind=c_int)            :: curl_easy_perform
        end function curl_easy_perform

        ! CURLcode curl_easy_setopt(CURL *curl, CURLoption option, ...)
        function curl_easy_setopt_(curl, option, parameter) bind(c, name='curl_easy_setopt')
            import :: c_int, c_ptr
            type(c_ptr),            intent(in), value :: curl
            integer(kind=c_int),    intent(in), value :: option
            type(c_ptr),            intent(in), value :: parameter
            integer(kind=c_int)                       :: curl_easy_setopt_
        end function curl_easy_setopt_

        ! struct curl_slist *curl_slist_append(struct curl_slist *list, const char *string)
        function curl_slist_append(list, string) bind(c, name='curl_slist_append')
            import :: c_char, c_ptr
            type(c_ptr),            intent(in), value :: list
            character(kind=c_char), intent(in)        :: string
            type(c_ptr)                               :: curl_slist_append
        end function curl_slist_append

        ! curl_version_info_data *curl_version_info(CURLversion age)
        function curl_version_info_(age) bind(c, name='curl_version_info')
            import :: c_int, c_ptr
            integer(kind=c_int), intent(in), value :: age
            type(c_ptr)                            :: curl_version_info_
        end function curl_version_info_

        ! void curl_easy_cleanup(CURL *curl)
        subroutine curl_easy_cleanup(curl) bind(c, name='curl_easy_cleanup')
            import :: c_ptr
            type(c_ptr), intent(in), value :: curl
        end subroutine curl_easy_cleanup

        ! void curl_slist_free_all(struct curl_slist *list)
        subroutine curl_slist_free_all(list) bind(c, name='curl_slist_free_all')
            import :: c_ptr
            type(c_ptr), intent(in), value :: list
        end subroutine curl_slist_free_all
    end interface

    interface
        ! int curl_version_now()
        function curl_version_now() bind(c, name='curl_version_now')
            !! Interface to wrapper function `curl_version_now()` for C constant
            !! `CURLVERSION_NOW`.
            import :: c_int
            integer(kind=c_int) :: curl_version_now
        end function curl_version_now
    end interface

    interface curl_easy_setopt
        ! Fortran 2008 generic interface `curl_easy_setopt()`.
        module procedure :: curl_easy_setopt_char
        module procedure :: curl_easy_setopt_fptr
        module procedure :: curl_easy_setopt_int
        module procedure :: curl_easy_setopt_long
        module procedure :: curl_easy_setopt_ptr
    end interface

    ! Fortran 2018 generic interface `curl_easy_setopt()`.
    ! generic :: curl_easy_setopt => curl_easy_setopt_char, curl_easy_setopt_fptr, curl_easy_setopt_int, &
    !                                curl_easy_setopt_long, curl_easy_setopt_ptr
contains
    ! CURLcode curl_easy_setopt(CURL *curl, CURLoption option, ...)
    function curl_easy_setopt_char(curl, option, parameter)
        type(c_ptr),              intent(in) :: curl
        integer,                  intent(in) :: option
        character(len=*), target, intent(in) :: parameter
        integer                              :: curl_easy_setopt_char

        curl_easy_setopt_char = curl_easy_setopt_(curl, option, c_loc(parameter))
    end function curl_easy_setopt_char

    ! CURLcode curl_easy_setopt(CURL *curl, CURLoption option, ...)
    function curl_easy_setopt_fptr(curl, option, parameter)
        type(c_ptr),    intent(in) :: curl
        integer,        intent(in) :: option
        type(c_funptr), intent(in),target :: parameter
        integer                    :: curl_easy_setopt_fptr

        curl_easy_setopt_fptr = curl_easy_setopt_(curl, option, c_loc(parameter))
    end function curl_easy_setopt_fptr

    ! CURLcode curl_easy_setopt(CURL *curl, CURLoption option, ...)
    function curl_easy_setopt_int(curl, option, parameter)
        type(c_ptr),             intent(in) :: curl
        integer    ,             intent(in) :: option
        integer(kind=4), target, intent(in) :: parameter
        integer                             :: curl_easy_setopt_int

        curl_easy_setopt_int = curl_easy_setopt_(curl, option, c_loc(parameter))
    end function curl_easy_setopt_int

    ! CURLcode curl_easy_setopt(CURL *curl, CURLoption option, ...)
    function curl_easy_setopt_long(curl, option, parameter)
        type(c_ptr),             intent(in) :: curl
        integer,                 intent(in) :: option
        integer(kind=8), target, intent(in) :: parameter
        integer                             :: curl_easy_setopt_long

        curl_easy_setopt_long = curl_easy_setopt_(curl, option, c_loc(parameter))
    end function curl_easy_setopt_long

    ! CURLcode curl_easy_setopt(CURL *curl, CURLoption option, ...)
    function curl_easy_setopt_ptr(curl, option, parameter)
        type(c_ptr), intent(in) :: curl
        integer,     intent(in) :: option
        type(c_ptr), intent(in) :: parameter
        integer                 :: curl_easy_setopt_ptr

        curl_easy_setopt_ptr = curl_easy_setopt_(curl, option, parameter)
    end function curl_easy_setopt_ptr

    ! curl_version_info_data *curl_version_info(CURLversion age)
    function curl_version_info(age)
        !! Wrapper for `curl_version_info_()` that converts the returned C
        !! pointer to Fortran pointer of derived type `curl_version` (a.k.a. C
        !! struct `curl_version_info_data`).
        integer, intent(in)         :: age
        type(curl_version), pointer :: curl_version_info
        type(c_ptr)                 :: ptr

        ptr = curl_version_info_(age)

        if (c_associated(ptr)) then
            call c_f_pointer(ptr, curl_version_info)
            return
        end if

        allocate (curl_version_info)
    end function curl_version_info

    subroutine c_f_str_ptr(c_str, f_str)
        !! Utility routine that copies a C string, passed as a C pointer, to a
        !! Fortran string.
        use, intrinsic :: iso_c_binding, only: c_associated, c_char, c_f_pointer, c_null_char, c_ptr
        type(c_ptr),      intent(in)           :: c_str
        character(len=*), intent(out)          :: f_str
        character(kind=c_char, len=1), pointer :: chars(:)
        integer                                :: i

        if (.not. c_associated(c_str)) then
            f_str = ' '
            return
        end if

        call c_f_pointer(c_str, chars, [ huge(0) ])
        i = 1

        do while (chars(i) /= c_null_char .and. i <= len(f_str))
            f_str(i:i) = chars(i)
            i = i + 1
        end do

        if (i < len(f_str)) f_str(i:) = ' '
    end subroutine c_f_str_ptr
end module M_curl
