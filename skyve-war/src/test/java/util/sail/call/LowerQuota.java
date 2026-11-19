package util.sail.call;

import org.openqa.selenium.JavascriptExecutor;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.StorageQuota;

/**
 * Lowers the storage quota of the origin of the page in front to what it uses now plus some head room, so writes to
 * CacheStorage and IndexedDB beyond it fail with {@code QuotaExceededError}. Until {@code restore}. The browser may
 * still report the real quota through {@code navigator.storage.estimate()} (Chrome does), so a test that needs the page
 * to see little room left stubs the estimate too.
 * <ul>
 * <li>{@code headroomKb}: how many more kilobytes (1024 bytes) may be written, 0 or more (required unless
 * 		{@code restore})</li>
 * <li>{@code restore}: {@code true} to put back the real quota</li>
 * </ul>
 * Its variable, if the step names one, is the quota set in bytes, or {@code restored}. Needs {@code STORAGE_QUOTA}.
 */
public final class LowerQuota extends AbstractSailCall {
	/** Takes headroomKb and restore. */
	public LowerQuota() {
		super("headroomKb", "restore");
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) {
		boolean restore = parameters.bool("restore", false);
		int headroom = restore ? 0 : parameters.requiredInteger("headroomKb", 0);
		StorageQuota quota = require(context, StorageQuota.class);
		JavascriptExecutor page = (JavascriptExecutor) context.driver();
		String origin = String.valueOf(page.executeScript("return location.origin;"));
		if (restore) {
			quota.clearQuotaOverride(origin);
			return "restored";
		}
		Object usage = page.executeAsyncScript("const done = arguments[arguments.length - 1];" +
												"navigator.storage.estimate().then(e => done(e.usage), () => done(-1));");
		long used = (usage instanceof Number number) ? number.longValue() : -1L;
		if (used < 0L) {
			throw new IllegalStateException(name() + " could not read how much storage " + origin + " uses: " + usage);
		}
		long bytes = used + (headroom * 1024L);
		quota.overrideQuota(origin, bytes);
		return String.valueOf(bytes);
	}
}
