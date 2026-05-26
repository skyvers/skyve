package org.skyve.metadata.model.document;

import java.util.List;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlType;

/**
 * A named uniqueness constraint applied to a document table or a collection joining table.
 *
 * <p>Constraints are declared in the document XML under {@code <uniqueConstraints>} or
 * in a collection's {@code <uniqueConstraints>}. The persistence layer maps each
 * constraint to a {@code UNIQUE} index on the relevant columns.
 *
 * <p>The {@link DocumentScope} determines whether uniqueness is enforced globally,
 * per-customer, per-data-group, or per-user. The message is displayed when a constraint
 * violation is detected during save.
 *
 * @see Document#getUniqueConstraints()
 */
public interface UniqueConstraint extends NamedMetaData, DecoratedMetaData {
	/**
	 * Defines the tenancy scope within which uniqueness is enforced.
	 *
	 * <p>The scope maps to a {@link org.skyve.metadata.user.DocumentPermissionScope} so
	 * that the constraint SQL respects Skyve's multi-tenant data partitioning.
	 */
	@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public static enum DocumentScope {
		/** Uniqueness is enforced across all tenants. */
		global(DocumentPermissionScope.global),
		
		/** Uniqueness is enforced within a single customer tenant. */
		customer(DocumentPermissionScope.customer),
		
		/** Uniqueness is enforced within a data group. */
		dataGroup(DocumentPermissionScope.dataGroup),
		
		/** Uniqueness is enforced for a single user. */
		user(DocumentPermissionScope.user);

		private DocumentPermissionScope scope;
		
		/**
		 * @param scope
		 */
		private DocumentScope(DocumentPermissionScope scope) {
			this.scope = scope;
		}

		/**
		 * Converts this scope to the equivalent {@link org.skyve.metadata.user.DocumentPermissionScope}.
		 *
		 * @return the corresponding permission scope; never {@code null}
		 */
		public DocumentPermissionScope toDocumentPermissionScope() {
			return scope;
		}
	}

	/**
	 * Returns the tenancy scope within which this constraint enforces uniqueness.
	 *
	 * @return the document scope; never {@code null}
	 */
	public DocumentScope getScope();
	
	/**
	 * Returns a human-readable description of this constraint.
	 *
	 * @return the description; may be {@code null}
	 */
	public String getDescription();
	
	/**
	 * Returns the localisedDescription.
	 * @return the result
	 */
	public default String getLocalisedDescription() {
		return Util.i18n(getDescription());
	}
	
	/**
	 * Returns the user-facing message displayed when this constraint is violated.
	 *
	 * @return the validation message; may be {@code null}
	 */
	public String getMessage();
	
	/**
	 * Returns the ordered list of document attribute names that together form the unique key.
	 *
	 * @return the field names; never {@code null}
	 */
	public List<String> getFieldNames();
}
