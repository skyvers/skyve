package org.skyve.metadata.router.fluent;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.metadata.repository.router.UxUiMetadata;

/**
 * Builds one UX/UI variant and its route list within router metadata.
 */
public class FluentUxUi {
	private UxUiMetadata uxui = null;
	
	/**
	 * Creates a builder with new empty UX/UI metadata.
	 */
	public FluentUxUi() {
		uxui = new UxUiMetadata();
	}

	/**
	 * Creates a builder around existing UX/UI metadata.
	 *
	 * @param uxui backing metadata
	 */
	public FluentUxUi(UxUiMetadata uxui) {
		this.uxui = uxui;
	}

	/**
	 * Copies UX/UI metadata into this builder.
	 *
	 * @param uxui source UX/UI metadata
	 * @return this builder
	 */
	public FluentUxUi from(@SuppressWarnings("hiding") UxUiMetadata uxui) {
		name(uxui.getName());
		uxui.getRoutes().forEach(r -> addRoute(new FluentRoute().from(r)));
		return this;
	}
	
	/**
	 * Sets the UX/UI name.
	 *
	 * @param name UX/UI name
	 * @return this builder
	 */
	public FluentUxUi name(String name) {
		uxui.setName(name);
		return this;
	}
	
	/**
	 * Adds one route definition.
	 *
	 * @param route route wrapper
	 * @return this builder
	 */
	public FluentUxUi addRoute(FluentRoute route) {
		uxui.getRoutes().add(route.get());
		return this;
	}
	
	/**
	 * Removes all routes matching the supplied outcome URL.
	 *
	 * @param outcomeUrl outcome URL to remove
	 * @return this builder
	 */
	public FluentUxUi removeRoutes(String outcomeUrl) {
		uxui.getRoutes().removeIf(r -> outcomeUrl.equals(r.getOutcomeUrl()));
		return this;
	}

	/**
	 * Finds routes by their outcome URL.
	 *
	 * @param outcomeUrl route outcome URL
	 * @return matching routes, possibly empty
	 */
	public List<FluentRoute> findRoutes(String outcomeUrl) {
		List<FluentRoute> result = new ArrayList<>(10);
		uxui.getRoutes().stream()
						.filter(r -> outcomeUrl.equals(r.getOutcomeUrl()))
						.forEach(r -> result.add(new FluentRoute(r)));
		return result;
	}
	
	/**
	 * Removes all routes from this UX/UI entry.
	 *
	 * @return this builder
	 */
	public FluentUxUi clearRoutes() {
		uxui.getRoutes().clear();
		return this;
	}

	/**
	 * Returns the mutable metadata instance being built.
	 *
	 * @return backing UX/UI metadata
	 */
	public UxUiMetadata get() {
		return uxui;
	}
}
