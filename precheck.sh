#!/bin/bash

sbt clean scalafmt Test/scalafmt coverage test coverageReport
