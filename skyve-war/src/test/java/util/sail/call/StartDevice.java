package util.sail.call;

import java.io.IOException;
import java.nio.file.Path;

import org.openqa.selenium.chromium.ChromiumDriver;
import org.opentest4j.TestAbortedException;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.BrowserConfiguration;
import util.sail.SailProfiles;
import util.sail.SailSettings;
import util.sail.Selenide;
import util.sail.browser.Capability;
import util.sail.browser.NetworkConditions;
import util.sail.browser.SailBrowserControl;
import util.sail.browser.TouchEmulation;

/**
 * Puts the leg on a device: a named browser profile, which keeps its IndexedDB, CacheStorage, localStorage, service
 * worker and persistent cookies (not session cookies) from the last leg on it, so a device can be put down, used by
 * someone else on another device, and picked up again with its unsent work. Use it first in a device's leg; the next
 * step is a navigate step.
 * <p>
 * The profile is the folder {@code <root>/<run token>/<profile>} that {@link SailProfiles} gives, with the root from
 * {@code sail.profiles} ({@code target/sail-profiles} under the working directory by default), so each run starts with
 * new devices; the run's folder is deleted when the run passes, and kept when it fails. If the leg's browser isn't on
 * that folder, it is stopped and started again on it, with the same settings; either way the device's network and touch
 * settings are applied afterwards, as a restarted browser has none.
 * <ul>
 * <li>{@code profile}: the device's name: letters, digits, {@code -} or {@code _}, such as {@code phone1} (required)</li>
 * <li>{@code offline}: {@code true} to take the device offline before any page opens, as a phone in a basement; it is
 * 		online by default</li>
 * <li>{@code touch}: {@code true} for a touch screen, so {@code (pointer: coarse)} matches; a mouse by default</li>
 * <li>{@code remembered}: {@code false} to start the device afresh, deleting what its profile kept; {@code true} (the
 * 		default) to keep it</li>
 * </ul>
 * Its variable, if the step names one, is the profile's folder.
 * <p>
 * Only Chrome and Edge take a profile: in another browser the test is skipped. {@code offline} needs
 * {@code NETWORK_CONDITIONS} and {@code touch} needs {@code TOUCH}, which Chrome and Edge have.
 */
public final class StartDevice extends AbstractSailCall {
	/** How many touch points a touch screen has. */
	private static final int TOUCH_POINTS = 5;

	/** Takes profile, offline, touch and remembered. */
	public StartDevice() {
		super("profile", "offline", "touch", "remembered");
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) throws IOException {
		String profile = parameters.required("profile").trim();
		if (! profile.matches("[A-Za-z0-9_-]+")) {
			throw new IllegalArgumentException(name() + "'s profile must be letters, digits, - or _, not " + profile);
		}
		boolean offline = parameters.bool("offline", false);
		boolean touch = parameters.bool("touch", false);
		boolean remembered = parameters.bool("remembered", true);
		requireProfiles(context, name());

		Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> selenide = context.selenide();
		BrowserConfiguration configuration = selenide.getConfiguration();
		SailSettings settings = selenide.getSettings();
		if ((configuration == null) || (settings == null)) {
			throw new IllegalStateException(name() + " runs in a leg whose browser has started");
		}
		Path folder = SailProfiles.folder(settings, context.runToken(), profile);
		if ((! remembered) || (! folder.equals(configuration.getProfile()))) {
			selenide.stopBrowser();
			if (! remembered) {
				SailProfiles.delete(folder);
			}
			selenide.startBrowser(configuration.profile(folder), settings);
		}
		applyDevice(context, name(), offline, touch);
		return folder.toString();
	}

	/**
	 * Skips the test when the leg's browser can't take a profile: only Chrome and Edge can.
	 *
	 * @param context	the leg
	 * @param call	the call, for the message
	 * @throws TestAbortedException	if the browser isn't Chrome or Edge
	 */
	static void requireProfiles(@Nonnull Context context, @Nonnull String call) {
		if (! (context.driver() instanceof ChromiumDriver)) {
			throw new TestAbortedException(call + " needs a device profile, which " + context.control().browser() + " doesn't support");
		}
	}

	/**
	 * Applies a device's network and touch settings to the leg's browser. A setting that the browser can't change is
	 * left alone when it asks for what the browser has anyway (online, and a mouse).
	 *
	 * @param context	the leg, with the browser that runs now
	 * @param call	the call, for a skip's message
	 * @param offline	true to take the browser offline, false to bring it online
	 * @param touch	true for a touch screen, false for a mouse
	 * @throws util.sail.browser.SailCapabilityUnavailableException	if the browser can't go offline or emulate touch, and
	 * 																	that was asked for
	 */
	static void applyDevice(@Nonnull Context context, @Nonnull String call, boolean offline, boolean touch) {
		SailBrowserControl control = context.control();
		if (offline) {
			control.require(NetworkConditions.class, call).offline();
		}
		else if (control.supports(Capability.NETWORK_CONDITIONS)) {
			control.require(NetworkConditions.class, call).online();
		}
		if (touch) {
			control.require(TouchEmulation.class, call).emulateTouch(TOUCH_POINTS);
		}
		else if (control.supports(Capability.TOUCH)) {
			control.require(TouchEmulation.class, call).emulateTouch(0);
		}
	}
}
