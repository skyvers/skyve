package org.skyve.metadata.router.fluent;

import org.skyve.impl.metadata.repository.router.RouteCriteria;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.web.WebAction;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Builds one route criteria block used to match an incoming request context.
 */
public class FluentRouteCriteria {
	private RouteCriteria criteria = null;
	
	/**
	 * Creates a builder with new empty route-criteria metadata.
	 */
	public FluentRouteCriteria() {
		criteria = new RouteCriteria();
	}

	/**
	 * Creates a builder around existing route-criteria metadata.
	 *
	 * @param criteria backing metadata
	 */
	public FluentRouteCriteria(@Nonnull RouteCriteria criteria) {
		this.criteria = criteria;
	}

	/**
	 * Copies route criteria metadata into this builder.
	 *
	 * @param criteria source criteria metadata
	 * @return this builder
	 */
	public @Nonnull FluentRouteCriteria from(@SuppressWarnings("hiding") @Nonnull RouteCriteria criteria) {
		viewType(criteria.getViewType());
		webAction(criteria.getWebAction());
		moduleName(criteria.getModuleName());
		documentName(criteria.getDocumentName());
		queryName(criteria.getQueryName());
		customerName(criteria.getCustomerName());
		dataGroupId(criteria.getDataGroupId());
		userId(criteria.getUserId());
		return this;
	}	
	
	/**
	 * Sets the matching view type.
	 *
	 * @param viewType view type
	 * @return this builder
	 */
	public @Nonnull FluentRouteCriteria viewType(@Nullable ViewType viewType) {
		criteria.setViewType(viewType);
		return this;
	}
	
	/**
	 * Sets the matching web action.
	 *
	 * @param webAction web action
	 * @return this builder
	 */
	public @Nonnull FluentRouteCriteria webAction(@Nullable WebAction webAction) {
		criteria.setWebAction(webAction);
		return this;
	}
	
	/**
	 * Sets the matching module name.
	 *
	 * @param moduleName module name
	 * @return this builder
	 */
	public @Nonnull FluentRouteCriteria moduleName(@Nullable String moduleName) {
		criteria.setModuleName(moduleName);
		return this;
	}
	
	/**
	 * Sets the matching document name.
	 *
	 * @param documentName document name
	 * @return this builder
	 */
	public @Nonnull FluentRouteCriteria documentName(@Nullable String documentName) {
		criteria.setDocumentName(documentName);
		return this;
	}
	
	/**
	 * Sets the matching query name.
	 *
	 * @param queryName query name
	 * @return this builder
	 */
	public @Nonnull FluentRouteCriteria queryName(@Nullable String queryName) {
		criteria.setQueryName(queryName);
		return this;
	}
	
	/**
	 * Sets the matching customer name.
	 *
	 * @param customerName customer name
	 * @return this builder
	 */
	public @Nonnull FluentRouteCriteria customerName(@Nullable String customerName) {
		criteria.setCustomerName(customerName);
		return this;
	}
	
	/**
	 * Sets the matching data-group identifier.
	 *
	 * @param dataGroupId data-group identifier
	 * @return this builder
	 */
	public @Nonnull FluentRouteCriteria dataGroupId(@Nullable String dataGroupId) {
		criteria.setDataGroupId(dataGroupId);
		return this;
	}
	
	/**
	 * Sets the matching user identifier.
	 *
	 * @param userId user identifier
	 * @return this builder
	 */
	public @Nonnull FluentRouteCriteria userId(@Nullable String userId) {
		criteria.setUserId(userId);
		return this;
	}

	/**
	 * Returns the mutable metadata instance being built.
	 *
	 * @return backing route criteria metadata
	 */
	public @Nonnull RouteCriteria get() {
		return criteria;
	}
}
