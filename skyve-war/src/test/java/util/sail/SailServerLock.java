package util.sail;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import jakarta.annotation.Nonnull;

/**
 * Serialises SAIL browser tests that run against the same server, across JVMs (e.g. surefire forks).
 * <p>
 * The admin suites back up, change, truncate and restore the server's data, so two of them (or a suite and any other
 * SAIL test) must never run against one server at the same time. Each SAIL IT takes this lock for its base URL before
 * its browser starts and holds it until the IT class has finished. The lock is an operating system file lock on
 * {@code skyve-sail-<base url>.lock} in {@code java.io.tmpdir}, so it holds across forked JVMs, and the operating system
 * releases it if a JVM dies.
 */
public final class SailServerLock {
	private static final Map<String, Held> HELD = new HashMap<>();

	private record Held(@Nonnull FileChannel channel, @Nonnull FileLock lock) {
	}

	private SailServerLock() {
		// static only
	}

	/**
	 * Takes the lock for a server, waiting while another JVM holds it. Does nothing if this JVM holds it already.
	 *
	 * @param baseUrl	the server's base URL
	 * @throws UncheckedIOException	if the lock file cannot be opened or locked
	 */
	public static synchronized void acquire(@Nonnull String baseUrl) {
		String key = key(baseUrl);
		if (HELD.containsKey(key)) {
			return;
		}
		Path file = Path.of(System.getProperty("java.io.tmpdir"), "skyve-sail-" + key + ".lock");
		try {
			FileChannel channel = FileChannel.open(file, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
			try {
				FileLock lock = channel.tryLock();
				if (lock == null) {
					System.out.println("SAIL: another test is using " + baseUrl + " - waiting for it to finish (" + file + ")");
					// The one necessary wait: another JVM's SAIL test is using this server, and must finish first
					lock = channel.lock();
				}
				HELD.put(key, new Held(channel, lock));
			}
			catch (IOException | RuntimeException e) {
				channel.close();
				throw e;
			}
		}
		catch (IOException e) {
			throw new UncheckedIOException("Could not lock " + file + " for SAIL tests against " + baseUrl, e);
		}
	}

	/**
	 * Releases every server lock this JVM holds.
	 */
	public static synchronized void releaseAll() {
		for (Held held : HELD.values()) {
			try {
				held.lock().release();
				held.channel().close();
			}
			catch (@SuppressWarnings("unused") IOException e) {
				// closing the channel (or the JVM ending) releases it anyway
			}
		}
		HELD.clear();
	}

	private static @Nonnull String key(@Nonnull String baseUrl) {
		String result = baseUrl.trim().toLowerCase(Locale.ROOT);
		while (result.endsWith("/")) {
			result = result.substring(0, result.length() - 1);
		}
		return result.replaceAll("[^a-z0-9]+", "_");
	}
}
