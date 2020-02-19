# erlang-course-pollution

Small project of server keeping information about air pollution done during Erlang course.

**Modules:**
  * **pollution** - module providing all functionalities - creating monitor, adding new data, getting daily mean values etc.
  * **pollution_tests** - tests for pollution module
  * **pollution_server** - manually created global variable server using pollution functionalities
  * **pollution_server_tests** - tests for pollution_server
  * **pollution_server_sup** - module restarting pollution_server after crash
  * **pollution_server_gen** - global variable server using gen_server pattern
  * **pollution_server_gen_tests** - tests for pollution_server_gen
  * **pollution_supervisor** - module implementing supervisor pattern, restarting server with latest saved monitor state
  * **pollution_monitor_keeper** - gen_server keeping backup of monitor
  * **pollution_sql_reader** - functions reading pollution data from database using odbc driver
  * **misc/** - other small modules done during classes
  * **elixir_lab/** - Elixir moduler done during Elixir class for reading pollution data from csv file with and without use of streams
