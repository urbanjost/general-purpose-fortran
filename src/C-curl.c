/* curlv.c */
#include <curl/curl.h>

/* Wrapper function that provides access to the constant `CURLVERSION_NOW`. */
int curl_version_now()
{
    return CURLVERSION_NOW;
}
