rm -rf $PLATFORM/generators/boom/src/main/scala/*
rm -rf $PLATFORM/generators/rocket-chip/src/main/scala/*
rm -rf $PLATFORM/generators/chipyard/src/main/scala/*

cp -r $MEEK/Hardware/big $PLATFORM/generators/boom/src/main/scala/
cp -r $MEEK/Hardware/little $PLATFORM/generators/rocket-chip/src/main/scala/
cp -r $MEEK/Hardware/top $PLATFORM/generators/chipyard/src/main/scala/