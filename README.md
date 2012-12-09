# elmine

~elmine~ is a simple package to interact with the redmine restful
webservice easily. Essentially it abstracts most API calls to the
Redmine API.

## Usage

To access the Redmine API you have to specifiy the redmine to access
and the API key to access it with. That´s either done via setting the
variables ~redmine/host~ and ~redmine/api-key~

    (setq elmine/host "https://www.my-redmine.org")
    (setq elmine/api-key "abcdefghijklmnopqrstuvwxyz1234567890")

or bind the variables ~redmine-host~ and ~redmine-api-key~
dynamically.

    (let ((redmine-host "https://www.my-redmine.org")
          (redmine-api-key "acdefghijklmnopqrstuvwxyz1234567890"))
      (redmine/get-issues))

## License

Copyright (C) 2012 Arthur Leonard Andersen

Authors: Arthur Leonard Andersen <leoc.git AT gmail.com>
Keywords: tools

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see http://www.gnu.org/licenses/.
