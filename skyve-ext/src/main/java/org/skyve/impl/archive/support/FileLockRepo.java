package org.skyve.impl.archive.support;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReentrantReadWriteLock;

/**
 * Singleton repository of per-file {@link java.util.concurrent.locks.ReentrantReadWriteLock}
 * instances that serialises concurrent read and write access to archive files.
 *
 * <p>Obtained via {@link #getInstance()}. Locks are keyed by the canonical file path
 * and created on first request.
 *
 * <p>Threading: the lock-creation path ({@link #getLockFor}) is {@code synchronized}
 * on the singleton; lock acquisition by callers is unsynchronised and follows the
 * standard {@link java.util.concurrent.locks.ReadWriteLock} contract.
 */
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