CC = $(ANDROID_BIN)/arm-linux-androideabi-gcc
ANDROID_DST = /data/local/tmp

%.exe: %.s
	$(CC) $^ -o $@

%.sim: %.exe
	adb push $^ $(ANDROID_DST)
	adb shell $(ANDROID_DST)/$^
