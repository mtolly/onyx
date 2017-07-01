.PHONY: build

build:
	g++ test.cpp \
		cbits/AvatarAsset/AssetHelpers.cpp \
		cbits/Cryptography/XeCrypt.cpp \
		cbits/Gpd/XdbfHelpers.cpp \
		cbits/IO/BaseIO.cpp \
		cbits/IO/FileIO.cpp \
		cbits/IO/MemoryIO.cpp \
		cbits/Stfs/StfsDefinitions.cpp \
		cbits/Stfs/StfsPackage.cpp \
		cbits/Stfs/XContentHeader.cpp \
		cbits/XboxInternals_onyx.cpp \
		-DXBOXINTERNALS_STATIC \
		$(shell pkg-config --cflags --libs botan-2) \
		-Icbits \
		-std=c++11 -Wall -g \
		-o test
