package org.skyve.impl.archive.support;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class FileLockRepo {

    private Map<String, ReentrantReadWriteLock> locks = new HashMap<>();

    private static final class SingletonHolder {
        private static final FileLockRepo INSTANCE = new FileLockRepo();
    }

    private FileLockRepo() {
    }

    public static FileLockRepo getInstance() {
        return SingletonHolder.INSTANCE;
    }

    public synchronized ReentrantReadWriteLock getLockFor(File file) throws IOException {

        return locks.computeIfAbsent(file.getCanonicalPath(), (k) -> new ReentrantReadWriteLock());
    }
}