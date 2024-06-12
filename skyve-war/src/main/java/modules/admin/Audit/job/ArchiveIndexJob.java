package modules.admin.Audit.job;

import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.toList;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.stream.Stream;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.skyve.job.CancellableJob;
import org.skyve.util.Util;

import jakarta.inject.Inject;
import modules.admin.Audit.job.support.FileLockRepo;

public class ArchiveIndexJob extends CancellableJob {

    private static final Logger logger = LogManager.getLogger();

    @Inject
    private FileLockRepo repo;

    @Override
    public void execute() throws Exception {

        List<Path> archives = emptyList();
        List<Lock> locks = new ArrayList<>();

        Path dir = Path.of(Util.getArchiveDirectory());

        try (Stream<Path> s = Files.list(dir)) {
            archives = s.collect(toList());
        }

        try {
            for (Path path : archives) {

                File f = path.toFile();
                Lock lock = repo.getLockFor(f)
                                .readLock();
                logger.info("Read locking {}", f.getName());

                lock.lock();
                locks.add(lock);
            }

            logger.info("Sleeping for 1 min");
            TimeUnit.MINUTES.sleep(1);
        } finally {
            locks.forEach(l -> {
                logger.info("Unlocking {}", l);
                l.unlock();
            });
        }

    }

}
