@echo off

rem firstboot.bat
rem Copyright (C) 2009-2010 Red Hat Inc.
rem
rem This program is free software; you can redistribute it and/or
rem modify it under the terms of the GNU Lesser General Public
rem License as published by the Free Software Foundation; either
rem version 2 of the License, or (at your option) any later version.
rem
rem This library is distributed in the hope that it will be useful,
rem but WITHOUT ANY WARRANTY; without even the implied warranty of
rem MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
rem Lesser General Public License for more details.
rem
rem You should have received a copy of the GNU Lesser General Public
rem License along with this library; if not, write to the Free Software
rem Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

echo v2v first boot script started > log.txt

echo installing rhev-apt >> log.txt
"rhev-apt.exe" /S /v/qn >>log.txt

echo starting rhev-apt
net start rhev-apt >>log.txt
