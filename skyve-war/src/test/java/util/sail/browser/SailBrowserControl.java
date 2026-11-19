package util.sail.browser;

import java.time.Duration;
import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

import org.openqa.selenium.Capabilities;
import org.openqa.selenium.HasCapabilities;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chromium.ChromiumDriver;
import org.openqa.selenium.remote.RemoteWebDriver;

import jakarta.annotation.Nonnull;

/**
 * Controls a browser beyond plain WebDriver, for SAIL steps written in Java: the entry point to the capability
 * interfaces ({@link Capability}). A step gets the control of its browser with {@link #of(WebDriver)}, asks whether
 * the browser has a capability with {@link #supports(Capability)}, and gets the capability's interface with
 * {@link #require(Class, String)}, which skips the test when the browser hasn't got it.
 * <p>
 * Each implementation implements the interfaces of the capabilities it supports. Which implementation a browser gets
 * depends on the browser: Chromium browsers (Chrome and Edge) get every capability
 * ({@link ChromiumBrowserControl}), any other browser what plain WebDriver can do ({@link WebDriverBrowserControl}).
 * <p>
 * Threading: {@link #of(WebDriver)} is thread-safe. A control is thread-confined, as its driver is.
 */
public abstract class SailBrowserControl {
	/**
	 * How often a SAIL condition wait checks again for an event that nothing signals, such as a new tab appearing, a
	 * request reaching the log or a page's script holding: as often as {@code Selenide.waitFor} checks a page. Such a
	 * wait polls because the browser raises no event the harness can wait on; it is bounded by a timeout.
	 */
	public static final Duration POLLING = Duration.ofMillis(100);

	/** The control of each driver, until the driver quits. Guarded by itself. */
	private static final Map<WebDriver, SailBrowserControl> CONTROLS = new IdentityHashMap<>();

	private final String browser;

	/**
	 * Names the browser this control controls, for the messages.
	 *
	 * @param browser	the browser's name, such as {@code chrome} or {@code firefox}
	 */
	protected SailBrowserControl(@Nonnull String browser) {
		this.browser = browser;
	}

	/**
	 * Returns the control of a browser, made the first time it is asked for, by browser. The control is made outside
	 * the lock, as a Chromium control connects to DevTools then, so one browser's slow start doesn't hold up another's
	 * steps; if two threads make one for the same driver at once, one is kept and the other released.
	 *
	 * <p>Side effects: forgets (and releases) the controls of drivers that have quit, so a browser started again (on the
	 * same profile, say) gets a new control, with none of the old one's settings. A quit driver's control is released
	 * here, at the next call for any driver, not when its driver quits; until then it holds its DevTools connection,
	 * whose thread is a daemon, so it never keeps the JVM running.
	 *
	 * <p>Complexity: O(d) in the number of drivers with a control, which is the number of browsers open.
	 *
	 * @param driver	the browser's driver
	 * @return the browser's control: the same one each time, until the driver quits
	 */
	public static @Nonnull SailBrowserControl of(@Nonnull WebDriver driver) {
		List<SailBrowserControl> quit = new ArrayList<>();
		SailBrowserControl result;
		synchronized (CONTROLS) {
			CONTROLS.entrySet().removeIf(entry -> {
				if (hasQuit(entry.getKey())) {
					quit.add(entry.getValue());
					return true;
				}
				return false;
			});
			result = CONTROLS.get(driver);
		}
		quit.forEach(SailBrowserControl::release);
		if (result != null) {
			return result;
		}
		SailBrowserControl made = create(driver);
		synchronized (CONTROLS) {
			result = CONTROLS.putIfAbsent(driver, made);
		}
		if (result != null) { // another thread made one first
			made.release();
			return result;
		}
		return made;
	}

	/** Makes the control for a driver, chosen by its browser. */
	private static @Nonnull SailBrowserControl create(@Nonnull WebDriver driver) {
		if (driver instanceof ChromiumDriver) {
			// Chrome and Edge
			return new ChromiumBrowserControl(driver);
		}
		return new WebDriverBrowserControl(driver);
	}

	/**
	 * Lets go of what this control holds in its browser beyond the driver, once the driver has quit: called when
	 * {@link #of(WebDriver)} forgets it.
	 *
	 * @implSpec	The default holds nothing, so does nothing.
	 */
	void release() {
		// plain WebDriver holds nothing beyond its driver
	}

	/** Indicates whether a driver has quit: a remote driver forgets its session id when it quits. */
	private static boolean hasQuit(@Nonnull WebDriver driver) {
		return (driver instanceof RemoteWebDriver remote) && (remote.getSessionId() == null);
	}

	/**
	 * Returns a driver's browser name, as its capabilities give it, or else the driver's class name.
	 *
	 * @param driver	the driver
	 * @return the name, such as {@code chrome}, {@code MicrosoftEdge}, {@code firefox} or {@code Safari}
	 */
	static @Nonnull String browserName(@Nonnull WebDriver driver) {
		if (driver instanceof HasCapabilities has) {
			Capabilities capabilities = has.getCapabilities();
			String name = (capabilities == null) ? null : capabilities.getBrowserName();
			if ((name != null) && (! name.isBlank())) {
				return name;
			}
		}
		return driver.getClass().getSimpleName();
	}

	/**
	 * Returns the name of the browser this control controls.
	 *
	 * @return the name, such as {@code chrome} or {@code firefox}
	 */
	public final @Nonnull String browser() {
		return browser;
	}

	/**
	 * Indicates whether the browser has a capability.
	 *
	 * @implSpec	The default is true when this control implements the capability's interface; an override may support
	 * 				fewer, never more.
	 *
	 * @param capability	the capability
	 * @return true if {@link #require(Class, String)} returns its interface, false if it skips the test
	 */
	public boolean supports(@Nonnull Capability capability) {
		return capability.type().isInstance(this);
	}

	/**
	 * Returns a capability's interface, or skips the test when the browser hasn't got the capability: a step that
	 * needs it can show nothing in this browser, so it hasn't failed.
	 *
	 * @param <T>	the interface's type
	 * @param type	the capability's interface, such as {@link NetworkConditions}{@code .class}
	 * @param call	the step that needs it, for the message, such as its simple class name
	 * @return the interface, for this browser
	 * @throws SailCapabilityUnavailableException	if the browser hasn't got the capability, which JUnit and the SAIL
	 * 												harness report as skipped
	 * @throws IllegalArgumentException	if the type is not a capability's interface
	 */
	public final @Nonnull <T> T require(@Nonnull Class<T> type, @Nonnull String call) {
		Capability capability = Capability.of(type);
		if (! supports(capability)) {
			throw new SailCapabilityUnavailableException(call, capability, browser);
		}
		return type.cast(this);
	}
}
