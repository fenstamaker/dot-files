#!/bin/bash
options=$(getopt -l "region::" -o "r::" -a -- "$@")
eval set -- "$options"

while true; do
    case $1 in
        -r|--region)
            shift
            export region=$1
            ;;
        --)
            shift
            break
            ;;
    esac
    shift
done

echo $region
echo $1


aws kms decrypt --ciphertext-blob fileb://<(echo "$input" | base64 --decode) --output text --query Plaintext | base64 --decode
