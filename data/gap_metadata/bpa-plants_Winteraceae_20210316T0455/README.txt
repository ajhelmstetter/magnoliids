CKAN Bulk Download
------------------

Search of organization: bpa-plants bpa-plants_Winteraceae_20210316T0455

Bulk download package generated:
2021-03-16T04:55:06.086822+0000

This archive contains the following files:

download.ps1:
Windows PowerShell script, which when executed will download the files,
and then checksum them. There are no dependencies other than PowerShell.

download.sh:
UNIX shell script, which when executed will download the files,
and then checksum them. This is supported on any Linux or MacOS/BSD
system, so long as `curl` is installed.

Before running either of these scripts, please set the CKAN_API_KEY
environment variable.

You can find your API Key by browsing to:
https://data.bioplatforms.com/user/hervesauquet

The API key has the format:
xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
To set the environment variable in Linux/MacOS/Unix, use:
export CKAN_API_KEY=xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx

On Microsoft Windows, within Powershell, use:
$env:CKAN_API_KEY=xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx

package_metadata folder:
Contains metadata spreadsheets for all selected data packages, grouped by
the type of package (schema). Each data package will contain one or more
resources. This metadata is an amalgamation of all metadata, including
sample contextual metadata and processing metadata.

resource_metadata folder:
Contains metadata spreadsheets for all selected data resources (files).

QUERY.txt:
Text file which contains metadata about the download results and the original
query

tmp folder:
This folder contains files required by the download scripts. Its
contents can be ignored.
