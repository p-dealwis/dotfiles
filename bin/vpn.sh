#!/usr/bin/env bash

sudo openvpn --config /etc/openvpn/client/conversions.conf --daemon && sudo openvpn --config /etc/openvpn/client/pramodya.conf --daemon
