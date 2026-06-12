package org.skyve.impl.metadata.repository.router;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SerializableMetaData;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.web.WebAction;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated criteria record used to match an incoming web request against a
 * {@link Route}.
 *
 * <p>A {@code RouteCriteria} specifies zero or more predicates — view type, web
 * action, module name, document name, query name, customer name, data-group ID,
 * or user ID — that must all match for the enclosing {@link Route} to apply.  A
 * {@code null} field means the criterion is unconstrained (matches any value).
 *
 * <p>Threading: not thread-safe.  Instances are populated during JAXB unmarshalling
 * and are read-only once placed in the repository cache.
 *
 * @see Route
 * @see Router
 */
@XmlType(namespace = XMLMetaData.ROUTER_NAMESPACE)
public class RouteCriteria implements SerializableMetaData {
	private static final long serialVersionUID = 7017356339189117479L;

	private ViewType viewType;
	private WebAction webAction;
	private String moduleName;
	private String documentName;
	private String queryName;
	private String customerName;
	private String dataGroupId;
	private String userId;

	/**
	 * Returns the constrained view type for matching.
	 *
	 * @return view type criterion, or {@code null}
	 */
	public ViewType getViewType() {
		return viewType;
	}

	/**
	 * Sets the constrained view type for matching.
	 *
	 * @param viewType view type criterion
	 */
	@XmlAttribute
	public void setViewType(ViewType viewType) {
		this.viewType = viewType;
	}
	
	/**
	 * Returns the constrained web action for matching.
	 *
	 * @return web action criterion, or {@code null}
	 */
	public WebAction getWebAction() {
		return webAction;
	}

	/**
	 * Sets the constrained web action for matching.
	 *
	 * @param webAction web action criterion
	 */
	@XmlAttribute
	public void setWebAction(WebAction webAction) {
		this.webAction = webAction;
	}
	
	/**
	 * Returns the constrained module name for matching.
	 *
	 * @return module name criterion, or {@code null}
	 */
	public String getModuleName() {
		return moduleName;
	}

	/**
	 * Sets the constrained module name for matching.
	 *
	 * @param moduleName module name criterion
	 */
	@XmlAttribute(name = "module")
	public void setModuleName(String moduleName) {
		this.moduleName = UtilImpl.processStringValue(moduleName);
	}

	/**
	 * Returns the constrained document name for matching.
	 *
	 * @return document name criterion, or {@code null}
	 */
	public String getDocumentName() {
		return documentName;
	}

	/**
	 * Sets the constrained document name for matching.
	 *
	 * @param documentName document name criterion
	 */
	@XmlAttribute(name = "document")
	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}

	/**
	 * Returns the constrained query name for matching.
	 *
	 * @return query name criterion, or {@code null}
	 */
	public String getQueryName() {
		return queryName;
	}

	/**
	 * Sets the constrained query name for matching.
	 *
	 * @param queryName query name criterion
	 */
	@XmlAttribute(name = "query")
	public void setQueryName(String queryName) {
		this.queryName = UtilImpl.processStringValue(queryName);
	}

	/**
	 * Returns the constrained customer name for matching.
	 *
	 * @return customer name criterion, or {@code null}
	 */
	public String getCustomerName() {
		return customerName;
	}

	/**
	 * Sets the constrained customer name for matching.
	 *
	 * @param customerName customer name criterion
	 */
	@XmlAttribute(name = "customer")
	public void setCustomerName(String customerName) {
		this.customerName = UtilImpl.processStringValue(customerName);
	}

	/**
	 * Returns the constrained data-group identifier for matching.
	 *
	 * @return data-group criterion, or {@code null}
	 */
	public String getDataGroupId() {
		return dataGroupId;
	}

	/**
	 * Sets the constrained data-group identifier for matching.
	 *
	 * @param dataGroupId data-group criterion
	 */
	@XmlAttribute
	public void setDataGroupId(String dataGroupId) {
		this.dataGroupId = UtilImpl.processStringValue(dataGroupId);
	}
	
	/**
	 * Returns the constrained user identifier for matching.
	 *
	 * @return user identifier criterion, or {@code null}
	 */
	public String getUserId() {
		return userId;
	}

	/**
	 * Sets the constrained user identifier for matching.
	 *
	 * @param userId user identifier criterion
	 */
	@XmlAttribute
	public void setUserId(String userId) {
		this.userId = UtilImpl.processStringValue(userId);
	}

	/**
	 * Canonicalises module/document criteria using a binding expression.
	 *
	 * <p>For non-map actions this resolves {@code binding} against the supplied or
	 * configured customer metadata and rewrites {@code moduleName}/{@code documentName}
	 * to the owning document of the resolved target.
	 *
	 * @param customer optional customer context; when {@code null} the configured
	 *        customer name is used
	 * @param binding the binding expression to resolve; blank values are ignored
	 * @throws IllegalStateException when canonicalisation is requested without
	 *         pre-set module or document names
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	public void canonicalise(Customer customer, String binding) {
		if (webAction != WebAction.m) { // exclude maps
			String b = UtilImpl.processStringValue(binding);
			if (b != null) {
				if (moduleName == null) {
					throw new IllegalStateException("RouteCriteria - Set moduleName before calling canonicalise()");
				}
				if (documentName == null) {
					throw new IllegalStateException("RouteCriteria - Set documentName before calling canonicalise()");
				}
				// Cater for FacesView.zoomInBindings stack written out as comma separated - convert to the view binding
				b = b.replace(',', '.');
	
				ProvidedRepository r = ProvidedRepositoryFactory.get();
				Customer c = (customer == null) ? ((customerName != null) ? r.getCustomer(customerName) : null) : customer;
				Module m = r.getModule(c, moduleName);
				Document d = r.getDocument(c, m, documentName);
				TargetMetaData t = BindUtil.getMetaDataForBinding(c, m, d, b);
				d = t.getDocument();
				moduleName = d.getOwningModuleName();
				documentName = d.getName();
				Attribute a = t.getAttribute();
				if (a instanceof Relation relation) {
					documentName = relation.getDocumentName();
					d = r.getDocument(c, m, documentName);
					moduleName = d.getOwningModuleName();
				}
			}
		}
	}
	
	/**
	 * Tests whether this criterion matches another criterion instance.
	 *
	 * <p>Each non-null field on this instance acts as a predicate that must equal
	 * the corresponding field on {@code criteria}. Null fields are wildcards.
	 *
	 * @param criteria the runtime request criteria to test
	 * @return {@code true} if all constrained fields match, otherwise {@code false}
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	public boolean matches(RouteCriteria criteria) {
		if ((customerName != null) && (! customerName.equals(criteria.customerName))) {
			return false;
		}
		if ((moduleName != null) && (! moduleName.equals(criteria.moduleName))) {
			return false;
		}
		if ((documentName != null) && (! documentName.equals(criteria.documentName))) {
			return false;
		}
		if ((queryName != null) && (! queryName.equals(criteria.queryName))) {
			return false;
		}
		if ((dataGroupId != null) && (! dataGroupId.equals(criteria.dataGroupId))) {
			return false;
		}
		if ((userId != null) && (! userId.equals(criteria.userId))) {
			return false;
		}
		if ((viewType != null) && (! viewType.equals(criteria.viewType))) {
			return false;
		}
		if ((webAction != null) && (! webAction.equals(criteria.webAction))) {
			return false;
		}

		return true;
	}
	
	/**
	 * Returns a concise string form containing all non-null criteria fields.
	 *
	 * @return criteria summary string
	 */
	@Override
	public String toString() {
		StringBuilder result = new StringBuilder(64);
		result.append('{');
		boolean something = false;
		if (customerName != null) {
			result.append("customerName=").append(customerName).append(',');
			something = true;
		}
		if (moduleName != null) {
			result.append("moduleName=").append(moduleName).append(',');
			something = true;
		}
		if (documentName != null) {
			result.append("documentName=").append(documentName).append(',');
			something = true;
		}
		if (queryName != null) {
			result.append("queryName=").append(queryName).append(',');
			something = true;
		}
		if (dataGroupId != null) {
			result.append("dataGroupId=").append(dataGroupId).append(',');
			something = true;
		}
		if (userId != null) {
			result.append("userId=").append(userId).append(',');
			something = true;
		}
		if (viewType != null) {
			result.append("viewType=").append(viewType).append(',');
			something = true;
		}
		if (webAction != null) {
			result.append("webAction=").append(webAction).append(',');
			something = true;
		}
		if (something) {
			result.setLength(result.length() - 1); // remove last comma
		}
		result.append('}');
		
		return result.toString();
	}
}
