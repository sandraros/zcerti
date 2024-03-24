# zcerti

Get HTTPS certificates from a Web site via the ICM trace (transaction code SMICM) and upload them to STRUST.

Initial idea from [here](https://github.com/abapGit/abapGit/issues/6720#issuecomment-1873742763) to read the ICM trace and parse the certificates,
and credits to [MBT here](https://github.com/abapGit/abapGit/issues/6720#issuecomment-1877376292) for uploading the certificates to STRUST.

Program `ZCERTI_DEMO`:

![image](https://github.com/sandraros/zcerti/assets/34005250/452b7791-0480-42b6-952f-35aaa7725680)

Excerpts of `ZCERTI_DEMO` source code:
- Start and parse the ICM trace to get the certificates:
  ```abap
  DATA(icm_trace_api) = zcl_certi_icm_trace=>create( ).
  DATA(original_trace_level) = icm_trace_api->get_trace_level( ).
  icm_trace_api->set_trace_level( '3' ). " highest details to get certificate contents
  icm_trace_api->delete_trace( ).
  DATA(parsed_icm_trace) = icm_trace_api->get_parsed_trace( ).
  icm_trace_api->set_trace_level( original_trace_level ).
  LOOP AT parsed_icm_trace-certificates REFERENCE INTO DATA(certificate).
    ...
  ENDLOOP.
  ```
- Update STRUST SSL Client PSE (`zcl_certi_mbt_strust` = exact copy of class [`/mbtools/cl_strust`](https://github.com/Marc-Bernard-Tools/MBT-Package-Manager/blob/main/src/core/%23mbtools%23cl_strust.clas.abap), GPL 3.0 License):
  ```abap
  DATA(strust) = NEW zcl_certi_mbt_strust( iv_context = 'SSLC' iv_applic  = 'ANONYM' ).
  strust->load( ).
  strust->get_own_certificate( ).
  DATA(certificates) = strust->get_certificate_list( ).
  LOOP AT parsed_icm_trace-certificates REFERENCE INTO certificate.
    strust->add( CONV #( certificate->lines ) ).
  ENDLOOP.
  strust->update( ).
  COMMIT WORK.
  ```
