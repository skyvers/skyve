package org.skyve.impl.archive.support;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock.ReadLock;
import java.util.concurrent.locks.ReentrantReadWriteLock.WriteLock;

import org.junit.Ignore;
import org.junit.Test;

public class FileLockRepoTest {

    private FileLockRepo repo = FileLockRepo.getInstance();

	@Test
    public void testBasics() throws IOException {

        File f = new File("a");
        ReentrantReadWriteLock rrwl = repo.getLockFor(f);

        ReadLock readlock = rrwl.readLock();
        WriteLock writeLock = rrwl.writeLock();

        assertTrue(readlock.tryLock());
        assertTrue(readlock.tryLock());

        // Upgrading from a read to a write is not possible
        assertFalse(writeLock.tryLock());
    }

    /**
     * Ensure that different (but equivalent) path representations receive the same lock.
     */
    @Test
    public void testCanonicalPath() throws IOException {

        File f1 = new File("b");
        ReentrantReadWriteLock rrwl1 = repo.getLockFor(f1);

        File f2 = new File("x/../b");
        ReentrantReadWriteLock rrwl2 = repo.getLockFor(f2);

        assertSame(rrwl1, rrwl2);
    }

    /**
     * In order to test the actual locking functionality we need the locks to be
     * acquired by different threads. So we'll spin up two ExecutorServices and
     * (try to) acquire the locks in threads on those.
     */
    @Test
    @Ignore("Works but is slow, and might be flaky")
    public void testLockConflict() throws InterruptedException, IOException, ExecutionException {

        ExecutorService pool1 = Executors.newSingleThreadExecutor();
        ExecutorService pool2 = Executors.newSingleThreadExecutor();
        CountDownLatch readLockAcquired = new CountDownLatch(1);
        CountDownLatch writeAttempted = new CountDownLatch(1);

        File f = new File("d");
        ReentrantReadWriteLock rwwl = repo.getLockFor(f);

        Future<String> f1 = pool1.submit(() -> {

            ReadLock readlock = rwwl.readLock();
            assertTrue(readlock.tryLock());
            readLockAcquired.countDown();
            assertTrue(writeAttempted.await(2, TimeUnit.SECONDS));
            readlock.unlock();

            return "1 done";
        });

        Future<String> f2 = pool2.submit(() -> {
            assertTrue(readLockAcquired.await(2, TimeUnit.SECONDS));

            WriteLock writeLock = rwwl.writeLock();
            assertFalse(writeLock.tryLock());
            writeAttempted.countDown();

            return "2 done";
        });

        f1.get();
        f2.get();

        pool1.awaitTermination(1, TimeUnit.SECONDS);
        pool2.awaitTermination(1, TimeUnit.SECONDS);
    }
}
