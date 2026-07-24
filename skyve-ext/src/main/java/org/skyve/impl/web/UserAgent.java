package org.skyve.impl.web;

import java.util.Collections;
import java.util.Objects;

import org.ehcache.Cache;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.cache.DefaultCaching;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.router.UxUiSelector;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.skyve.web.UserAgentType;
import org.slf4j.Logger;

import com.blueconic.browscap.BrowsCapField;
import com.blueconic.browscap.Capabilities;
import com.blueconic.browscap.UserAgentParser;
import com.blueconic.browscap.UserAgentService;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

/**
 * Detects request device capabilities and progressively resolves one request selection.
 *
 * <p>Selection uses one fixed precedence order: validated emulation, direct router metadata, then
 * the configured selector's normal fallback policy. Effective device detection and the completed
 * selection share one progressively populated selection object in a private request attribute.
 *
 * <p>Thread-safe: static shared state is immutable or concurrency-safe. Selection creation is
 * request-confined and occurs before asynchronous request processing begins.
 */
public final class UserAgent {
	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(UserAgent.class);

	/**
	 * Key for finding the UserAgent in the request object.
	 */
	private static final String REQUEST_SELECTION_KEY = UserAgent.class.getName() + ".selection";

	/**
	 * The BrowsCap parse instance.
	 */
	private static final UserAgentParser PARSER;
	// NB Initialised here so its thread safe.
	static {
		try {
			LOGGER.info("Load BrowsCap");
			PARSER = new UserAgentService().loadParser(Collections.singleton(BrowsCapField.DEVICE_TYPE));
			LOGGER.info("Loaded BrowsCap");
		}
		catch (Exception e) {
			throw new DomainException("Cannot initialise Browscap.", e);
		}
	}

	/**
	 * Prevents instantiation.
	 */
	private UserAgent() {
		// Utility class.
	}

	/**
	 * Ensures the BrowsCap parser is initialised.
	 *
	 * <p>Side effects: first active use loads the BrowsCap device data.
	 */
	public static void init() {
		// nothing to do here as its done in the thread safe static initialiser
	}

	/**
	 * Detects the effective device type without selecting a UX/UI.
	 *
	 * <p>This method and {@link #getSelection(HttpServletRequest)} may be called in either order.
	 * On first use, this method snapshots the validated emulation input or physical device detection
	 * in the request's single selection object. Repeated calls reuse that value, and a later
	 * {@code getSelection()} progressively completes the same object. When selection is already
	 * complete, this method returns its effective type.
	 *
	 * <p>This method deliberately does not invoke router matching or the configured UX/UI selector,
	 * because their result can depend on path, principal, and session state established later in the
	 * request. Changes to device/emulation inputs after the first {@code UserAgent} lookup are not
	 * observed.
	 *
	 * This is called from RequestLoggingAndStatisticsFilter and direct JSPs.
	 *
	 * @param request current request
	 * @return validated effective type
	 */
	public static @Nonnull UserAgentType detectType(@Nonnull HttpServletRequest request) {
		return requestSelection(request).getUserAgentType();
	}

	/**
	 * Returns the request's completed selection, resolving its UX/UI once when required.
	 *
	 * <p>This method and {@link #detectType(HttpServletRequest)} may be called in either order. If no
	 * partial selection exists, this method initialises one before resolving the UX/UI; otherwise it
	 * reuses the effective type and emulation flag already detected. A later {@code detectType()}
	 * returns the type from this completed selection.
	 *
	 * <p>The first call to this method is the UX/UI lifecycle freeze point: router matching and
	 * selector policy observe the path, principal, and session state visible at that moment. The
	 * resulting {@link RequestUxUiSelection} identity is authoritative for the rest of the request.
	 * Callers may detect the device earlier, but must not request full selection before the state on
	 * which their selector depends has been established.
	 *
	 * <p>Validated session emulation is evaluated before direct metadata. The one-time
	 * {@code _ua} command accepted by {@code device.jsp} can select only a device type and can never
	 * name a UX/UI.
	 *
	 * @param request current request; must not be {@code null}
	 * @return current request selection; never {@code null}
	 * @throws DomainException if no router is configured
	 * @throws MetaDataException if a matched trusted name cannot resolve to a UX/UI
	 */
	public static @Nonnull RequestUxUiSelection getSelection(@Nonnull HttpServletRequest request) {
		RequestUxUiSelection result = requestSelection(request);
		if (! result.isComplete()) {
			result.complete(resolveUxUi(request, result));
		}
		return result;
	}

	/**
	 * Returns the request's progressive selection or initialises effective device detection.
	 *
	 * @param request current request; must not be {@code null}
	 * @return request-owned progressive selection; never {@code null}
	 * @throws IllegalStateException if the private attribute has an unexpected value
	 */
	private static @Nonnull RequestUxUiSelection requestSelection(@Nonnull HttpServletRequest request) {
		RequestUxUiSelection result = (RequestUxUiSelection) request.getAttribute(REQUEST_SELECTION_KEY);
		if (result == null) {
			UserAgentType emulated = emulatedType(request);
			if (emulated == null) {
				result = new RequestUxUiSelection(detectPhysicalType(request), false);
			}
			else {
				result = new RequestUxUiSelection(emulated, true);
			}
			request.setAttribute(REQUEST_SELECTION_KEY, result);
		}
		return result;
	}

	/**
	 * Resolves a new selection using emulation, direct metadata, then normal selector precedence.
	 *
	 * <p>Side effects: the winning selector operation may inspect request, session, user, or
	 * application state. This method does not store the resulting selection on the request.
	 *
	 * @param request current request; must not be {@code null}
	 * @return selected application UX/UI; never {@code null}
	 * @throws DomainException if no router is configured
	 * @throws MetaDataException if a matched direct name cannot be resolved
	 */
	private static @Nonnull UxUi resolveUxUi(@Nonnull HttpServletRequest request,
												@Nonnull RequestUxUiSelection selection) {
		Router router = router();
		UxUiSelector selector = (UxUiSelector) router.getUxuiSelector();
		UserAgentType userAgentType = selection.getUserAgentType();
		if (selection.isEmulated()) {
			return selector.emulate(userAgentType, request);
		}

		String directName = router.selectDirect(normalisedTarget(request), userAgentType);
		if (directName != null) {
			// Framework boundry
			return Objects.requireNonNull(selector.resolve(directName), "UX/UI selector cannot resolve trusted metadata name " + directName);
		}
		// Framework boundary
		return Objects.requireNonNull(selector.select(userAgentType, request), "UX/UI selector selected yielded null");
	}

	/**
	 * Returns the configured effective router.
	 *
	 * @return repository router; never {@code null}
	 * @throws DomainException if the repository has no router
	 */
	private static @Nonnull Router router() {
		Router result = CORE.getRepository().getRouter();
		if (result == null) {
			throw new DomainException("No router configured; cannot resolve UX/UI");
		}
		return result;
	}

	/**
	 * Returns the optional device-emulation selection stored in the current HTTP session.
	 *
	 * <p>The preview selection is deliberately session-scoped so navigation, postbacks, and
	 * background requests share one selection without carrying a URL parameter. No session is
	 * created while detecting a physical device.
	 *
	 * @param request current request; must not be {@code null}
	 * @return requested device type, or {@code null} when emulation is inactive
	 */
	private static @Nullable UserAgentType emulatedType(@Nonnull HttpServletRequest request) {
		HttpSession session = request.getSession(false);
		if (session == null) {
			return null;
		}
		return (UserAgentType) session.getAttribute(AbstractWebContext.EMULATED_USER_AGENT_TYPE_SESSION_ATTRIBUTE_NAME);
	}

	/**
	 * Consumes the one-time device-preview command from a {@code device.jsp} request.
	 *
	 * <p>A valid {@code UserAgentType} enables or changes preview mode. A blank value removes preview
	 * mode. The caller must redirect after consuming the command because request selection may
	 * already have been detected by an earlier filter.
	 *
	 * <p>Side effects: creates an HTTP session when enabling preview mode, or mutates the existing
	 * session when changing or ending it.
	 *
	 * @param request current device command request; must not be {@code null}
	 * @throws IllegalArgumentException when the command is not a valid device type
	 */
	public static void consumeDevicePreviewCommand(@Nonnull HttpServletRequest request) {
		String value = UtilImpl.processStringValue(request.getParameter(AbstractWebContext.EMULATED_USER_AGENT_TYPE_PARAMETER));
		if (value == null) {
			HttpSession session = request.getSession(false);
			if (session != null) {
				session.removeAttribute(AbstractWebContext.EMULATED_USER_AGENT_TYPE_SESSION_ATTRIBUTE_NAME);
			}
			return;
		}

		try {
			UserAgentType type = UserAgentType.valueOf(value);
			request.getSession(true).setAttribute(AbstractWebContext.EMULATED_USER_AGENT_TYPE_SESSION_ATTRIBUTE_NAME, type);
		}
		catch (Exception e) {
			throw new IllegalArgumentException("Invalid device-preview user-agent type", e);
		}
	}

	/**
	 * Builds the context-relative target used by direct router matching.
	 *
	 * <p>The servlet path and path info are concatenated, query and fragment suffixes are removed,
	 * and an empty result is normalised to {@code /}. Forward-source attributes are deliberately
	 * ignored, so the current dispatch target is used.
	 *
	 * <p>Complexity: O(n) time and O(n) space, where n is the combined path length.
	 *
	 * @param request current request
	 * @return normalized target beginning with {@code /}
	 */
	private static @Nonnull String normalisedTarget(@Nonnull HttpServletRequest request) {
		String servletPath = request.getServletPath();
		String pathInfo = request.getPathInfo();
		StringBuilder target = new StringBuilder();
		if (servletPath != null) {
			target.append(servletPath);
		}
		if (pathInfo != null) {
			target.append(pathInfo);
		}
		if (target.isEmpty()) {
			return "/";
		}
		int query = target.indexOf("?");
		int fragment = target.indexOf("#");
		int end = query;
		if ((end < 0) || ((fragment >= 0) && (fragment < end))) {
			end = fragment;
		}
		String result = (end < 0) ? target.toString() : target.substring(0, end);
		return result.isEmpty() ? "/" : result;
	}

	/**
	 * Detects the physical device type from the touch cookie and User-Agent header.
	 *
	 * <p>Side effects: caches the BrowsCap result by User-Agent header. Desktop is promoted to
	 * tablet when the request carries {@code touch=1}; that request-specific promotion is not
	 * cached.
	 *
	 * @param request current request; must not be {@code null}
	 * @return detected physical device type; never {@code null}
	 */
	private static @Nonnull UserAgentType detectPhysicalType(@Nonnull HttpServletRequest request) {
		boolean touchEnabled = touchEnabled(request);
		String agentString = UtilImpl.processStringValue(request.getHeader("User-Agent"));
		if (agentString == null) {
			agentString = "";
		}
		Cache<String, UserAgentType> typeCache = EXT.getCaching().getEHCache(DefaultCaching.USER_AGENT_TYPE_CACHE_NAME,
																				String.class,
																				UserAgentType.class);
		UserAgentType result = typeCache.get(agentString);
		if (result == null) {
			result = parsePhysicalType(agentString);
			typeCache.put(agentString, result);
		}
		return ((result == UserAgentType.desktop) && touchEnabled) ? UserAgentType.tablet : result;
	}

	/**
	 * Indicates whether the request carries the enabled touch-capability cookie.
	 *
	 * <p>Complexity: O(c) time and O(1) space, where c is the request cookie count.
	 *
	 * @param request current request; must not be {@code null}
	 * @return {@code true} only when a {@code touch=1} cookie is present
	 */
	private static boolean touchEnabled(@Nonnull HttpServletRequest request) {
		Cookie[] cookies = request.getCookies();
		if (cookies != null) {
			for (int i = 0, length = cookies.length; i < length; i++) {
				Cookie cookie = cookies[i];
				if ("touch".equals(cookie.getName()) && "1".equals(cookie.getValue())) {
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Classifies a normalised User-Agent header using the shared BrowsCap parser.
	 *
	 * @param agentString normalised header value
	 * @return mapped Skyve device type, or {@link UserAgentType#other} when unrecognised
	 */
	private static @Nonnull UserAgentType parsePhysicalType(@Nonnull String agentString) {
		Capabilities capabilities = PARSER.parse(agentString);
		if (capabilities != null) {
			String deviceType = capabilities.getDeviceType();
			if ("Desktop".equals(deviceType)) {
				return UserAgentType.desktop;
			}
			if ("Tablet".equals(deviceType)) {
				return UserAgentType.tablet;
			}
			if ((deviceType != null) && deviceType.startsWith("Mobile")) {
				return UserAgentType.phone;
			}
		}
		return UserAgentType.other;
	}
}
