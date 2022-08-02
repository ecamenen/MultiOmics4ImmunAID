#!/bin/bash
TOOL_NAME="MultiOmics4ImmunAID"
TOOL_VERSION="0.0.1"
DOCKER_NAME="multi-aid"
DOCKER_VERSION="1.0.1"

docker build -t ${DOCKER_NAME}:${DOCKER_VERSION} --build-arg TOOL_NAME=${TOOL_NAME} --build-arg TOOL_VERSION=${TOOL_VERSION} .
