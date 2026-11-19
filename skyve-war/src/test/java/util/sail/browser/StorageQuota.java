package util.sail.browser;

import jakarta.annotation.Nonnull;

/**
 * An origin's storage quota, so a test can fill it.
 * <p>
 * The capability {@link Capability#STORAGE_QUOTA}.
 */
public interface StorageQuota {
	/**
	 * Sets the storage quota of an origin. Writes to CacheStorage and IndexedDB beyond it fail with
	 * {@code QuotaExceededError}, but {@code navigator.storage.estimate()} may still report the real quota (Chrome 158
	 * does), so a test that needs the page to see little room left stubs the estimate too. IndexedDB may compress what
	 * it stores, so fill it with data that doesn't compress.
	 *
	 * @param origin	the origin, such as {@code http://localhost:8080}
	 * @param bytes	the quota
	 */
	void overrideQuota(@Nonnull String origin, long bytes);

	/**
	 * Puts back the real storage quota of an origin.
	 *
	 * @param origin	the origin
	 */
	void clearQuotaOverride(@Nonnull String origin);
}
