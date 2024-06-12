package modules.admin.Audit.job.support;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import jakarta.inject.Singleton;

@Singleton
public class FileLockRepo {

    private Map<String, ReentrantReadWriteLock> locks = new HashMap<>();

    public synchronized ReentrantReadWriteLock getLockFor(File file) throws IOException {

        return locks.computeIfAbsent(file.getCanonicalPath(), (k) -> new ReentrantReadWriteLock());
    }
}