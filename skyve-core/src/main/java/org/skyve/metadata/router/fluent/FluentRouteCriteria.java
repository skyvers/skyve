package org.skyve.metadata.router.fluent;

import org.skyve.impl.metadata.repository.router.RouteCriteria;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.web.WebAction;

public class FluentRouteCriteria {
	private RouteCriteria criteria = null;
	
	public FluentRouteCriteria() {
		criteria = new RouteCriteria();
	}

	public FluentRouteCriteria(RouteCriteria criteria) {
		this.criteria = criteria;
	}

	public FluentRouteCriteria from(@SuppressWarnings("hiding") RouteCriteria criteria) {
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
	
	public FluentRouteCriteria viewType(ViewType viewType) {
		criteria.setViewType(viewType);
		return this;
	}
	
	public FluentRouteCriteria webAction(WebAction webAction) {
		criteria.setWebAction(webAction);
		return this;
	}
	
	public FluentRouteCriteria moduleName(String moduleName) {
		criteria.setModuleName(moduleName);
		return this;
	}
	
	public FluentRouteCriteria documentName(String documentName) {
		criteria.setDocumentName(documentName);
		return this;
	}
	
	public FluentRouteCriteria queryName(String queryName) {
		criteria.setQueryName(queryName);
		return this;
	}
	
	public FluentRouteCriteria customerName(String customerName) {
		criteria.setCustomerName(customerName);
		return this;
	}
	
	public FluentRouteCriteria dataGroupId(String dataGroupId) {
		criteria.setDataGroupId(dataGroupId);
		return this;
	}
	
	public FluentRouteCriteria userId(String userId) {
		criteria.setUserId(userId);
		return this;
	}

	public RouteCriteria get() {
		return criteria;
	}
}
