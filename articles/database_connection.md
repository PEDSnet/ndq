# Database Connection & Environment Setup

The NDQ package utilizes the `argos` system for interacting with data
and accessing remote databases. To best utilize this tool, you will need
to set up your environment to comply with the `argos` standards. Read
more about the argos package and what is required for environment setup
[here](https://github.com/PEDSnet/argos).

## Connecting to your CDM

The primary configuration in `argos` required to establish a database
connection is `config('db_src')`. You can set this environment variable
in one of two ways:

#### Option 1: DBI (or similar)

One option is to create a connection object inside your R session using
`DBI` or a similar database connection package. Instructions for how to
use [`DBI::dbConnect`](https://dbi.r-dbi.org/reference/dbConnect.html)
to establish a connection can be found in [the DBI package
documentation](https://dbi.r-dbi.org/articles/DBI.html).

If this option is used, you can set your environment variable as
`config('db_src', myDBIobject)`

#### Option 2: External JSON Configuration

Another option is to store your configuration details in a local JSON
file and feed path to that file into `srcr`. We have provided a simple
example below. As with Option 1, the type of information that would be
included in the file differs for different database backends.

    {
        "src_name" : "Postgres",
        "src_args" : {
                "host"     : "my.database.server",
                "port"     : 5432,
                "dbname"   : "project_db",
                "username" : "my_username",
                "password" : "my_password",
                "options"  : "-c search_path=my_cdm_schema"
         },
         "post_connect_sql" : [
             "set role project_staff;"
         ]
         
    }

If this option is used, you can set your environment variable as
`config('db_src', srcr('path/to/my/file'))`

## Custom Configuration

The NDQ package also utilizes a custom environment variable,
`config('qry_site')`, required only by this package and not `argos` as a
whole. You should set it to the name of the institution for which you
are executing the function(s), like
`config('qry_site', 'my_institution')`.

This configuration will ensure that, when applicable, only information
from one institution is being read during the execution of each
`check_*` function. This is done to improve performance and reduce the
amount of data that is being processed at once. If you would like to
execute these functions for multiple institutions, simply change the
variable to another institution and re-execute the analysis. The
`process_*` functions can be used to compute overall information based
on the combined results of each execution.
