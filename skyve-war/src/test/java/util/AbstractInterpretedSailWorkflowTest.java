package util;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;

import util.sail.SailServerLock;

/**
 * Tests the class end of {@link AbstractInterpretedSailWorkflow}: every test's workflow is closed, even when closing
 * one fails, and the server lock is released either way.
 */
@SuppressWarnings("static-method")
class AbstractInterpretedSailWorkflowTest {
	@Test
	void everyWorkflowIsClosedAndTheLockReleasedWhenAClosingFails() {
		List<String> closed = new ArrayList<>();
		IllegalStateException first = new IllegalStateException("the first browser had crashed");
		IllegalStateException second = new IllegalStateException("the second browser had crashed");
		AbstractInterpretedSailWorkflow.WORKFLOWS.add(() -> {
			closed.add("1");
			throw first;
		});
		AbstractInterpretedSailWorkflow.WORKFLOWS.add(() -> closed.add("2"));
		AbstractInterpretedSailWorkflow.WORKFLOWS.add(() -> {
			closed.add("3");
			throw second;
		});
		String baseUrl = "http://localhost:1/abstract-interpreted-sail-workflow-test";
		SailServerLock.acquire(baseUrl);

		IllegalStateException e = assertThrows(IllegalStateException.class, AbstractInterpretedSailWorkflow::closeWorkflowsAndReleaseServer);

		assertAll(() -> assertEquals(List.of("1", "2", "3"), closed, "each workflow is closed"),
					() -> assertSame(first, e, "the first failure is thrown"),
					() -> assertArrayEquals(new Throwable[] {second}, e.getSuppressed(), "with the others suppressed"),
					() -> assertTrue(AbstractInterpretedSailWorkflow.WORKFLOWS.isEmpty(), "none is left to close"),
					() -> assertTrue(released(baseUrl), "the server lock is released"));
	}

	/** Indicates whether this JVM can lock a server's lock file, as it can't while it holds the server lock. */
	private static boolean released(String baseUrl) throws IOException {
		// the lock file SailServerLock uses for the base URL
		Path file = Path.of(System.getProperty("java.io.tmpdir"), "skyve-sail-" + baseUrl.replaceAll("[^a-z0-9]+", "_") + ".lock");
		try (FileChannel channel = FileChannel.open(file, StandardOpenOption.WRITE);
				FileLock lock = channel.tryLock()) { // throws OverlappingFileLockException while this JVM holds it
			assertNotNull(lock, "no other JVM holds it");
			return true;
		}
		catch (@SuppressWarnings("unused") java.nio.channels.OverlappingFileLockException e) {
			return false;
		}
	}
}
