package org.skyve.impl.metadata.repository.router;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.skyve.impl.metadata.repository.ConvertibleMetaData;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.repository.router.Direct.DirectMatch;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.ReloadableMetaData;
import org.skyve.util.Util;
import org.skyve.web.UserAgentType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlRootElement(namespace = XMLMetaData.ROUTER_NAMESPACE)
@XmlType(namespace = XMLMetaData.ROUTER_NAMESPACE, propOrder = {"uxuis", "directs", "unsecuredUrlPrefixes", "properties"})
/**
 * JAXB root element for the router descriptor ({@code router.xml}).
 *
 * <p>A {@code Router} contains an ordered list of {@link Route} instances and an
 * optional set of UI ({@link UxUiMetadata}) configurations.  The router is loaded
 * once during application startup and used by the web layer to map incoming
 * requests to outcome URLs or UI configurations.
 *
 * <p>Threading: not thread-safe.  The instance is written during JAXB unmarshalling
 * and is read-only once placed in the repository cache.
 *
 * @see Route
 * @see Direct
 * @see UxUiMetadata
 * @see org.skyve.metadata.router.UxUi
 */
public class Router implements ConvertibleMetaData<Router>, DecoratedMetaData, ReloadableMetaData {
	private static final long serialVersionUID = 670690452538129424L;

	private long lastModifiedMillis = Long.MAX_VALUE;
	private long lastCheckedMillis = System.currentTimeMillis();
	
	@XmlElement(namespace = XMLMetaData.ROUTER_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Returns the source metadata last-modified timestamp used for reload checks.
	 *
	 * @return source metadata last-modified timestamp in milliseconds
	 */
	@Override
	public long getLastModifiedMillis() {
		return lastModifiedMillis;
	}

	/**
	 * Sets the source metadata last-modified timestamp used for reload checks.
	 *
	 * @param lastModifiedMillis source metadata last-modified timestamp in milliseconds
	 */
	@XmlTransient
	public void setLastModifiedMillis(long lastModifiedMillis) {
		this.lastModifiedMillis = lastModifiedMillis;
	}

	/**
	 * Returns the timestamp of the last repository check for this router metadata.
	 *
	 * @return last checked timestamp in milliseconds
	 */
	@Override
	public long getLastCheckedMillis() {
		return lastCheckedMillis;
	}

	/**
	 * Updates the timestamp of the last repository check for this router metadata.
	 *
	 * @param lastCheckedMillis last checked timestamp in milliseconds
	 */
	@Override
	@XmlTransient
	public void setLastCheckedMillis(long lastCheckedMillis) {
		this.lastCheckedMillis = lastCheckedMillis;
	}

	private String uxuiSelectorClassName;
	private TaggingUxUiSelector uxuiSelector;
	
	@XmlElement(namespace = XMLMetaData.ROUTER_NAMESPACE, name = "uxui")
	private List<UxUiMetadata> uxuis = new ArrayList<>();

	@XmlTransient
	private Map<String, UxUiMetadata> uxuiMap = new TreeMap<>();

	@XmlElement(namespace = XMLMetaData.ROUTER_NAMESPACE, name = "direct")
	private List<Direct> directs = new ArrayList<>();

	@XmlElement(namespace = XMLMetaData.ROUTER_NAMESPACE, name = "unsecured")
	private Set<String> unsecuredUrlPrefixes = new TreeSet<>();

	/**
	 * Returns configured URL prefixes that bypass security checks.
	 *
	 * @return mutable set of unsecured URL prefixes
	 */
	public Set<String> getUnsecuredUrlPrefixes() {
		return unsecuredUrlPrefixes;
	}

	/**
	 * Returns the fully-qualified class name of the UX/UI selector implementation.
	 *
	 * @return selector implementation class name, or {@code null}
	 */
	public String getUxuiSelectorClassName() {
		return uxuiSelectorClassName;
	}

	/**
	 * Sets the fully-qualified class name of the UX/UI selector implementation.
	 *
	 * @param uxuiSelectorClassName selector implementation class name
	 */
	@XmlAttribute(name = "uxuiSelectorClassName")
	public void setUxuiSelectorClassName(String uxuiSelectorClassName) {
		this.uxuiSelectorClassName = uxuiSelectorClassName;
	}

	/**
	 * Returns decorator properties defined for this router descriptor.
	 *
	 * @return mutable router property map
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
	
	/**
	 * Returns the lazily-instantiated UX/UI selector implementation.
	 *
	 * <p>Side effects: loads and instantiates {@code uxuiSelectorClassName} via the
	 * thread context class loader on first call.
	 *
	 * @return the selector instance, never {@code null}
	 * @throws MetaDataException if the selector class cannot be loaded or instantiated
	 */
	public TaggingUxUiSelector getUxuiSelector() {
		if (uxuiSelector == null) {
			try {
				Class<?> type = Thread.currentThread().getContextClassLoader().loadClass(uxuiSelectorClassName);
				uxuiSelector = (TaggingUxUiSelector) type.getDeclaredConstructor().newInstance();
			}
			catch (Exception e) {
				throw new MetaDataException("Cannot load UX/UI selector class " + uxuiSelectorClassName, e);
			}
		}
		return uxuiSelector;
	}

	/**
	 * Returns configured UX/UI metadata entries in declaration order.
	 *
	 * @return mutable list of UX/UI metadata entries
	 */
	public List<UxUiMetadata> getUxUis() {
		return uxuis;
	}

	/**
	 * Returns direct declarations in effective first-match order.
	 *
	 * <p>The mutable list retains duplicates and overlaps. Module merges prepend their complete
	 * declaration list, so later module merges precede earlier module merges and all module
	 * declarations precede global declarations.
	 *
	 * @return the mutable ordered list; never {@code null}
	 * @since 10.0
	 */
	public @Nonnull List<Direct> getDirects() {
		return directs;
	}

	/**
	 * Selects the UX/UI name from the first direct declaration matching all supplied inputs.
	 *
	 * <p>Precondition: {@code normalisedPath} is a context-relative normalised target beginning
	 * with {@code /}, and {@code userAgentType} is the already validated effective device category.
	 * Exact declarations are not reordered ahead of prefixes; absent declaration conditions are
	 * wildcards.
	 *
	 * <p>Complexity: O(d) time and O(1) auxiliary space, where d is the number of declarations
	 * examined before the first match.
	 *
	 * <p>Threading: safe for concurrent calls only after metadata publication while the returned
	 * declaration list is not mutated.
	 *
	 * @param normalisedPath normalised context-relative target path; must not be {@code null}
	 * @param userAgentType validated effective device category; must not be {@code null}
	 * @return the matching configured UX/UI name, or {@code null} when no declaration matches
	 * @since 10.0
	 */
	public @Nullable String selectDirect(@Nonnull String normalisedPath,
											@Nonnull UserAgentType userAgentType) {
		for (Direct direct : directs) {
			String declaredPath = direct.getPath();
			boolean pathMatches = (direct.getMatch() == DirectMatch.prefix)
									? normalisedPath.startsWith(declaredPath)
									: normalisedPath.equals(declaredPath);
			if (pathMatches) {
				UserAgentType directUserAgentType = direct.getUserAgentType();
				if ((directUserAgentType == null) || (directUserAgentType == userAgentType)) {
					return direct.getUxui();
				}
			}
		}
		return null;
	}

	/**
	 * Validates every direct target against this completely assembled router.
	 *
	 * <p>Precondition: all standalone router files have been converted and merged. This check is
	 * intentionally separate from {@link #convert(String)} so global and module routers remain
	 * independently convertible assembly components.
	 *
	 * <p>Complexity: O(d log u) time and O(1) auxiliary space, where d is the number of direct
	 * declarations and u is the number of effective UX/UI definitions.
	 *
	 * @param metaDataName metadata name used to identify configuration errors; must not be
	 *                     {@code null}
	 * @throws MetaDataException if a declaration names no effective UX/UI definition
	 * @since 10.0
	 */
	public void validateDirectTargets() {
		for (Direct direct : directs) {
			String uxuiName = direct.getUxui();
			if (! uxuiMap.containsKey(uxuiName)) {
				throw new MetaDataException("Router direct for path " + direct.getPath()
												+ " references unknown UX/UI " + uxuiName + '.');
			}
		}
	}

	/**
	 * Selects the first matching route outcome URL for the supplied UX/UI and criteria.
	 *
	 * @param uxui the UX/UI name to evaluate
	 * @param criteria request criteria to match
	 * @return the matched outcome URL, or {@code null} when no route matches
	 */
	public String selectOutcomeUrl(String uxui, RouteCriteria criteria) {
		Route route = selectRoute(uxui, criteria);
		return (route == null) ? null : route.getOutcomeUrl();
	}
	
	/**
	 * Selects the first route that matches the supplied UX/UI and request criteria.
	 *
	 * <p>Routes are evaluated in declaration order. A route with no criteria is
	 * treated as a match-all fallback for that UX/UI.
	 *
	 * @param uxui the UX/UI name to evaluate
	 * @param criteria request criteria to match
	 * @return the first matching route, or {@code null} if none match
	 */
	public Route selectRoute(String uxui, RouteCriteria criteria) {
		UxUiMetadata selectedUxUi = uxuiMap.get(uxui);
		if (selectedUxUi != null) {
			for (Route route : selectedUxUi.getRoutes()) {
				List<RouteCriteria> routeCriteria = route.getCriteria();
				if (routeCriteria.isEmpty()) { // if no criteria defined then its a match
					return route;
				}
				for (RouteCriteria routeCriterion : routeCriteria) {
					if (routeCriterion.matches(criteria)) {
						return route;
					}
				}
			}
		}
		
		return null;
	}
	
	/**
	 * Determines whether a URL prefix is exempt from security checks.
	 *
	 * @param urlPrefixToTest the request URL prefix to test
	 * @return {@code true} when the prefix starts with any configured unsecured
	 *         prefix, otherwise {@code false}
	 */
	public boolean isUnsecured(String urlPrefixToTest) {
		if (urlPrefixToTest != null) {
			for (String unsecuredURLPrefix : unsecuredUrlPrefixes) {
	        	if (urlPrefixToTest.startsWith(unsecuredURLPrefix)) {
	        		return true;
	        	}
	        }
		}
        return false;
	}
	
	/**
	 * Normalises this router after JAXB unmarshalling.
	 *
	 * <p>Side effects: rebuilds the internal UX/UI lookup map, applies direct defaults,
	 * validates declaration-local direct constraints, and trims unsecured URL prefixes while
	 * discarding blank entries. Effective direct target references are deliberately not
	 * checked until {@link #validateDirectTargets(String)} runs after assembly.
	 *
	 * @param metaDataName metadata path used in error reporting
	 * @return this router instance
	 * @throws MetaDataException if a direct declaration is locally invalid
	 */
	@Override
	@SuppressWarnings("java:S3776") // Complexity OK
	public @Nonnull Router convert(@Nonnull String metaDataName) {
		// populate the UX/UI map
		uxuiMap.clear();
		for (UxUiMetadata uxui : uxuis) {
			uxuiMap.put(uxui.getName(), uxui);
		}

		for (Direct direct : directs) {
			String path = direct.getPath();
			if (path == null) {
				throw new MetaDataException(metaDataName + " direct path is required.");
			}
			if (! path.startsWith("/")) {
				throw new MetaDataException(metaDataName + " direct path " + path
												+ " must be context-relative and begin with '/'.");
			}
			if ((path.indexOf('?') >= 0) || (path.indexOf('#') >= 0)) {
				throw new MetaDataException(metaDataName + " direct path " + path
												+ " must not contain query or fragment content.");
			}
			if ((direct.getMatch() == DirectMatch.prefix) && (! path.endsWith("/"))) {
				throw new MetaDataException(metaDataName + " direct prefix path " + path
												+ " must end with '/'.");
			}
			if (direct.getUxui() == null) {
				throw new MetaDataException(metaDataName + " direct UX/UI target is required for path " + path + '.');
			}
		}
		
		// trim the unsecured URL prefixes if required
		for (String unsecuredUrlPrefix : unsecuredUrlPrefixes) {
			String trimmed = Util.processStringValue(unsecuredUrlPrefix);
			if (trimmed == null) {
				unsecuredUrlPrefixes.remove(unsecuredUrlPrefix);
			}
			else if (trimmed.length() < unsecuredUrlPrefix.length()) {
				unsecuredUrlPrefixes.remove(unsecuredUrlPrefix);
				unsecuredUrlPrefixes.add(trimmed);
			}
		}
		
		return this;
	}
	
	/**
	 * Merges one converted module router into this effective router.
	 *
	 * <p>Existing UX/UI routes receive incoming routes at the front. Incoming direct
	 * declarations are likewise prepended as one ordered block, preserving duplicates and their
	 * internal declaration order. Consequently a later module merge precedes an earlier module
	 * merge, and module declarations precede global declarations.
	 *
	 * <p>Side effects: mutates this router's UX/UI definitions, lookup map, direct list,
	 * unsecured prefixes, and last-modified timestamp. The incoming router remains
	 * structurally owned by the caller, although its mutable metadata children are retained by
	 * reference.
	 *
	 * <p>Threading: not thread-safe; merge only during repository assembly before publication.
	 *
	 * @param routerToMerge converted module router to merge; must not be {@code null}
	 * @throws MetaDataException if a route merged into an existing UX/UI has criteria without a
	 *                           module name
	 * @throws NullPointerException if {@code routerToMerge} is {@code null}
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	public void merge(@Nonnull Router routerToMerge) {
		// Set the last modified to the latest timestamp of any router to be merged
		setLastModifiedMillis(Math.max(getLastModifiedMillis(), routerToMerge.getLastModifiedMillis()));

		// Merge UX/UIs
		final List<UxUiMetadata> uxuiMetadata = routerToMerge.getUxUis();
		final List<UxUiMetadata> newUxUis = new ArrayList<>();
		for (UxUiMetadata uxuiMetadatum : uxuiMetadata) {
			final UxUiMetadata existingUxuiMetadatum = uxuis.stream()
					.filter(uxui -> Objects.equals(uxui.getName(), uxuiMetadatum.getName()))
					.findFirst().orElse(null);
			if (existingUxuiMetadatum == null) {
				newUxUis.add(uxuiMetadatum);
			}
			else {
				// Module routers should not mask the generic routes from the global router.
				// To this end we ensure that at least the module criteria is defined.
				List<Route> routesToMerge = uxuiMetadatum.getRoutes();
				for (Route route : routesToMerge) {
					for (RouteCriteria criteria : route.getCriteria()) {
						if (criteria.getModuleName() == null) {
							throw new MetaDataException("Merged router with UX/UI " + uxuiMetadatum.getName() + " and outcomeUrl" + route.getOutcomeUrl() +
															" has a criteria without a module name defined." +
															" Module routers should be for the module when adding to existing UX/UI definitions." +
															" Add the non-module route to the global router.");
						}
					}
				}

				// Insert the routes at the front and merge the properties in
				existingUxuiMetadatum.getRoutes().addAll(0, uxuiMetadatum.getRoutes());
				existingUxuiMetadatum.getProperties().putAll(uxuiMetadatum.getProperties());
			}
		}
		uxuis.addAll(newUxUis);
		for (UxUiMetadata newUxUi : newUxUis) {
			uxuiMap.put(newUxUi.getName(), newUxUi);
		}

		// Prepend module declarations with the same precedence used by merged routes.
		directs.addAll(0, routerToMerge.getDirects());

		// Add any extra unsecured URL prefixes
		unsecuredUrlPrefixes.addAll(routerToMerge.getUnsecuredUrlPrefixes());
	}
}
