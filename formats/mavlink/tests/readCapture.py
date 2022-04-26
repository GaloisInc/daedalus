#!/usr/bin/env python3

from scapy.all import *
from scapy.utils import PcapReader
import os, sys

MAVLINK_MAGIC = [0xfd, 0xfe]

# converting a string of hex (form: 'deadbeef') into an integer list to make it easier to work with
def HexStringToIntList(hex_string):
    if len(hex_string) % 2 != 0:
        raise ValueError('hex_string is not a valid hex string!')
        exit(1)

    int_list = []
    for i in range(0, len(hex_string), 2):
        byte = hex_string[i:i+2]
        num = int(byte, 16)
        int_list.append(num)
    return int_list

# determine if the payload is mavlink by checking the first byte of the payload against the known MAVLink magic numbers
def IsMavlink(payload):
    if HexStringToIntList(payload)[0] in MAVLINK_MAGIC:
        return True

    return False

# wrapper for reading the pcap file with scapy's rdpcap()
def ReadPCAP(pcap_file):
    print("Reading from .pcap file: %s" % pcap_file)
    pcap = rdpcap(pcap_file)
    return pcap

# iterate over each packet in the pcap and determine if its MAVLink - if so, print the payload data
def ExtractPCAPContents(pcap_object):
    out = open('payloads', 'wb')
    for packet in pcap_object:
        if UDP in packet and Raw in packet:
#            payload_hex = bytes(packet[UDP].payload).encode('hex')
            bs = bytes(packet[UDP].payload)
            payload_hex = bs.hex()
            if IsMavlink(payload_hex):
                out.write(bs)
                out.write('\n'.encode('utf-8'))
                print(payload_hex)
    out.close()

# Gather cmd line arguments and run pcap reader
def main():
    if len(sys.argv) == 2:
        filepath = sys.argv[1]
        if os.path.exists(filepath) and os.path.isfile(filepath):
            try:
                pcap = ReadPCAP(filepath)
            except Exception as e:
                print("Error: Unable to read pcap file...\n%s" % str(e))
                exit(1)
            try:
                ExtractPCAPContents(pcap)
            except Exception as e:
                print("Error: Unable to parse pcap file...\n%s" % str(e))
                exit(1)
        else:
            print("Path to packet capture is either not a file, or does not exist!")
    else:
        print("Please put the path to the pcap file as the argument to the script!")
        

if __name__ == '__main__':
    main()
