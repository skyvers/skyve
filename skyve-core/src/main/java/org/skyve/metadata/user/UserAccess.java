package org.skyve.metadata.user;

import java.io.Serializable;
import java.util.Collections;
import java.util.Set;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

/**
 * An immutable value object that identifies a specific type of access being requested
 * to a Skyve resource, used by the router and security layer to determine whether the
 * current user may access a given URL or resource.
 *
 * <p>{@code UserAccess} encodes the resource type (singular edit, list aggregate, query
 * aggregate, model aggregate, previous-complete, report, dynamic image, or content)
 * together with the module, document, and optional component name required to locate
 * the resource. The type is encoded internally as a single character for efficient
 * serialisation.
 *
 * <p>Instances are created via the static factory methods ({@link #singular},
 * {@link #documentAggregate}, {@link #queryAggregate}, etc.) and compared by their
 * string representation via {@link #equals}, {@link #hashCode}, and
 * {@link #compareTo}.
 *
 * <p>The constant {@link #ALL_UX_UIS} (an empty {@code Set}) is used as a sentinel
 * meaning "applicable to all UX/UI variants".
 *
 * @see User#canAccess(UserAccess, String)
 */
public class UserAccess implements Serializable, Comparable<UserAccess> {
	private static final long serialVersionUID = 103193454372180990L;

	public static final Set<String> ALL_UX_UIS = Collections.emptySet();

	private char type;
	private String moduleName;
	private String documentName;
	private String component;
	
	private UserAccess(char type, String moduleName, String documentName, String component) {
		this.type = type;
		this.moduleName = moduleName;
		this.documentName = documentName;
		this.component = component;
	}

	/**
	 * Creates a {@code UserAccess} for a singular edit-view resource (one document instance).
	 *
	 * @param moduleName    the module that owns the document; must not be {@code null}
	 * @param documentName  the document name; must not be {@code null}
	 * @return a new singular {@code UserAccess}; never {@code null}
	 */
	public static UserAccess singular(String moduleName, String documentName) {
		return new UserAccess('S', moduleName, documentName, null);
	}
	
	/**
	 * Creates a {@code UserAccess} for a document-driven aggregate list resource.
	 *
	 * @param moduleName    the module that owns the list; must not be {@code null}
	 * @param documentName  the document name used as the aggregate target; must not be {@code null}
	 * @return a new document-aggregate {@code UserAccess}; never {@code null}
	 */
	public static UserAccess documentAggregate(String moduleName, String documentName) {
		return new UserAccess('D', moduleName, null, documentName);
	}

	/**
	 * Creates a {@code UserAccess} for a named-query aggregate list resource.
	 *
	 * @param moduleName  the module that declares the query; must not be {@code null}
	 * @param queryName   the query name; must not be {@code null}
	 * @return a new query-aggregate {@code UserAccess}; never {@code null}
	 */
	public static UserAccess queryAggregate(String moduleName, String queryName) {
		return new UserAccess('Q', moduleName, null, queryName);
	}

	/**
	 * Creates a {@code UserAccess} for a model-driven aggregate list resource.
	 *
	 * @param moduleName    the module that owns the document; must not be {@code null}
	 * @param documentName  the document name; must not be {@code null}
	 * @param modelName     the model name; must not be {@code null}
	 * @return a new model-aggregate {@code UserAccess}; never {@code null}
	 */
	public static UserAccess modelAggregate(String moduleName, String documentName, String modelName) {
		return new UserAccess('M', moduleName, documentName, modelName);
	}

	/**
	 * Creates a {@code UserAccess} for a previous/complete conversation resource.
	 *
	 * @param moduleName    the module; must not be {@code null}
	 * @param documentName  the document name; must not be {@code null}
	 * @param binding       the binding path; must not be {@code null}
	 * @return a new previous-complete {@code UserAccess}; never {@code null}
	 */
	public static UserAccess previousComplete(String moduleName, String documentName, String binding) {
		return new UserAccess('P', moduleName, documentName, binding);
	}

	/**
	 * Creates a {@code UserAccess} for a named report resource.
	 *
	 * @param moduleName    the module that owns the document; must not be {@code null}
	 * @param documentName  the document name; must not be {@code null}
	 * @param reportName    the report name; must not be {@code null}
	 * @return a new report {@code UserAccess}; never {@code null}
	 */
	public static UserAccess report(String moduleName, String documentName, String reportName) {
		return new UserAccess('R', moduleName, documentName, reportName);
	}

	/**
	 * Creates a {@code UserAccess} for a dynamic image resource.
	 *
	 * @param moduleName          the module that owns the document; must not be {@code null}
	 * @param documentName        the document name; must not be {@code null}
	 * @param dynamicImageName    the dynamic image name; must not be {@code null}
	 * @return a new dynamic-image {@code UserAccess}; never {@code null}
	 */
	public static UserAccess dynamicImage(String moduleName, String documentName, String dynamicImageName) {
		return new UserAccess('I', moduleName, documentName, dynamicImageName);
	}

	/**
	 * Creates a {@code UserAccess} for a content attachment resource.
	 *
	 * @param moduleName    the module that owns the document; must not be {@code null}
	 * @param documentName  the document name; must not be {@code null}
	 * @param binding       the attribute binding of the content field; must not be {@code null}
	 * @return a new content {@code UserAccess}; never {@code null}
	 */
	public static UserAccess content(String moduleName, String documentName, String binding) {
		return new UserAccess('C', moduleName, documentName, binding);
	}

	/** Returns {@code true} if this is a singular edit-view access request. */
	public boolean isSingular() {
		return (type == 'S');
	}

	/** Returns {@code true} if this is a document-driven aggregate list access request. */
	public boolean isDocumentAggregate() {
		return (type == 'D');
	}

	/** Returns {@code true} if this is a named-query aggregate list access request. */
	public boolean isQueryAggregate() {
		return (type == 'Q');
	}

	/** Returns {@code true} if this is a model-driven aggregate list access request. */
	public boolean isModelAggregate() {
		return (type == 'M');
	}

	/** Returns {@code true} if this is a previous/complete conversation access request. */
	public boolean isPreviousComplete() {
		return (type == 'P');
	}

	/** Returns {@code true} if this is a report access request. */
	public boolean isReport() {
		return (type == 'R');
	}

	/** Returns {@code true} if this is a dynamic image access request. */
	public boolean isDynamicImage() {
		return (type == 'I');
	}

	/** Returns {@code true} if this is a content attachment access request. */
	public boolean isContent() {
		return (type == 'C');
	}

	/**
	 * Returns the module name component of this access key.
	 *
	 * @return the module name; never {@code null}
	 */
	public String getModuleName() {
		return moduleName;
	}

	/**
	 * Returns the document name component of this access key, or {@code null} for
	 * query-aggregate accesses where there is no specific document.
	 *
	 * @return the document name, or {@code null}
	 */
	public String getDocumentName() {
		return documentName;
	}
	
	/**
	 * Returns the component name (query, model, report, dynamic image, or binding) for
	 * this access key, or {@code null} for singular accesses.
	 *
	 * @return the component name, or {@code null}
	 */
	public String getComponent() {
		return component;
	}
	
	/**
	 * For a {@link #isSingular() singular} access, walks the document inheritance chain
	 * to find the base (non-extending) document and returns a singular access for it.
	 *
	 * <p>Returns {@code null} if the document does not extend another document.
	 *
	 * @return a singular access for the base document, or {@code null} if none
	 */
	public UserAccess determineSingularBaseDocument() {
		Customer c = CORE.getCustomer();
		Module m = c.getModule(moduleName);
		Document d = m.getDocument(c, documentName);
		Extends e = d.getExtends();
		if (e == null) {
			return null;
		}
		String baseDocumentName = e.getDocumentName();
		d = m.getDocument(c, baseDocumentName);
		return UserAccess.singular(d.getOwningModuleName(), baseDocumentName);
	}
	
	@Override
	public boolean equals(Object other) {
		if (other == null) {
			return false;
		}
		return toString().equals(other.toString());
	}
	
	@Override
	public int hashCode() {
		return toString().hashCode();
	}
	
	@Override
	public String toString() {
		StringBuilder result = new StringBuilder(64);
		result.append(type);
		result.append(moduleName);
		if (documentName != null) {
			result.append('.');
			result.append(documentName);
		}
		if (component != null) {
			result.append('^');
			result.append(component);
		}		
		return result.toString();
	}
	
	@Override
	public int compareTo(UserAccess other) {
		return toString().compareTo(other.toString());
	}
}
