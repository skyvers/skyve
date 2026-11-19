package util.sail.call;

import java.nio.file.Path;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.BrowserConfiguration;
import util.sail.SailSettings;
import util.sail.Selenide;

/**
 * Stops the leg's browser and starts it again on the same device profile, as a phone whose browser was closed or killed
 * and then opened again: the device keeps its IndexedDB, CacheStorage, localStorage, service worker and persistent
 * cookies (a remember-me cookie), and loses its session cookies, its open pages and every setting made through the
 * browser's control (the network, request rules, init scripts). The device's network and touch settings are applied
 * again, as {@link StartDevice} does. The leg must be on a device profile already ({@link StartDevice}); the next step
 * is a navigate step.
 * <ul>
 * <li>{@code offline}: {@code true} to start the device offline, before any page opens; online by default</li>
 * <li>{@code touch}: {@code true} for a touch screen; a mouse by default</li>
 * </ul>
 * Nothing is signed in afterwards: a device signed in with remember-me is signed in again by its next request once it is
 * online. Its variable, if the step names one, is the profile's folder.
 * <p>
 * Only Chrome and Edge take a profile: in another browser the test is skipped.
 */
public final class RestartDevice extends AbstractSailCall {
	/** Takes offline and touch. */
	public RestartDevice() {
		super("offline", "touch");
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) {
		boolean offline = parameters.bool("offline", false);
		boolean touch = parameters.bool("touch", false);
		StartDevice.requireProfiles(context, name());

		Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> selenide = context.selenide();
		BrowserConfiguration configuration = selenide.getConfiguration();
		SailSettings settings = selenide.getSettings();
		Path profile = (configuration == null) ? null : configuration.getProfile();
		if ((configuration == null) || (settings == null) || (profile == null)) {
			throw new IllegalStateException(name() + " needs the leg on a device profile: use StartDevice first");
		}
		selenide.stopBrowser();
		selenide.startBrowser(configuration, settings);
		StartDevice.applyDevice(context, name(), offline, touch);
		return profile.toString();
	}
}
