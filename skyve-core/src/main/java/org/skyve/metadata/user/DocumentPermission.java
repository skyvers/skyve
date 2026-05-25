package org.skyve.metadata.user;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * An enum encoding the Create/Read/Update/Delete permissions and the data visibility
 * scope for a document, as declared in Skyve module role metadata.
 *
 * <p>Each constant name encodes five characters:
 * <pre>
 *   Position 0 &mdash; {@code C} = can create,  {@code _} = cannot
 *   Position 1 &mdash; {@code R} = can read,    {@code _} = cannot
 *   Position 2 &mdash; {@code U} = can update,  {@code _} = cannot
 *   Position 3 &mdash; {@code D} = can delete,  {@code _} = cannot
 *   Position 4 &mdash; scope: {@code G}=global, {@code C}=customer,
 *                              {@code D}=dataGroup, {@code U}=user, {@code _}=none
 * </pre>
 *
 * <p>For example, {@code CRU_G} means create/read/update (no delete) with global scope,
 * and {@code _____} means no permissions at all.
 *
 * <p>Permissions from multiple roles are combined via {@link #mergePermission},
 * which unions the CRUD bits and takes the broader (higher-ordinal) scope.
 *
 * @see DocumentPermissionScope
 * @see User
 */
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
public enum DocumentPermission {
	_____(false, false, false, false, DocumentPermissionScope.none), 
	
	CRUDG(true, true, true, true, DocumentPermissionScope.global),
	CRU_G(true, true, true, false, DocumentPermissionScope.global),
	CR__G(true, true, false, false, DocumentPermissionScope.global), 
	_R__G(false, true, false, false, DocumentPermissionScope.global), 
	_RU_G(false, true, true, false, DocumentPermissionScope.global), 
	_RUDG(false, true, true, true, DocumentPermissionScope.global), 
	_R_DG(false, true, false, true, DocumentPermissionScope.global), 
	CR_DG(true, true, false, true, DocumentPermissionScope.global),

	CRUDC(true, true, true, true, DocumentPermissionScope.customer),
	CRU_C(true, true, true, false, DocumentPermissionScope.customer), 
	CR__C(true, true, false, false, DocumentPermissionScope.customer), 
	_R__C(false, true, false, false, DocumentPermissionScope.customer), 
	_RU_C(false, true, true, false, DocumentPermissionScope.customer), 
	_RUDC(false, true, true, true, DocumentPermissionScope.customer), 
	_R_DC(false, true, false, true, DocumentPermissionScope.customer), 

	CRUDD(true, true, true, true, DocumentPermissionScope.dataGroup), 
	CRU_D(true, true, true, false, DocumentPermissionScope.dataGroup), 
	CR__D(true, true, false, false, DocumentPermissionScope.dataGroup), 
	_R__D(false, true, false, false, DocumentPermissionScope.dataGroup), 
	_RU_D(false, true, true, false, DocumentPermissionScope.dataGroup), 
	_RUDD(false, true, true, true, DocumentPermissionScope.dataGroup), 
	_R_DD(false, true, false, true, DocumentPermissionScope.dataGroup), 
	CR_DD(true, true, false, true, DocumentPermissionScope.dataGroup),

	CRUDU(true, true, true, true, DocumentPermissionScope.user), 
	CRU_U(true, true, true, false, DocumentPermissionScope.user), 
	CR__U(true, true, false, false, DocumentPermissionScope.user), 
	_R__U(false, true, false, false, DocumentPermissionScope.user), 
	_RU_U(false, true, true, false, DocumentPermissionScope.user), 
	_RUDU(false, true, true, true, DocumentPermissionScope.user), 
	_R_DU(false, true, false, true, DocumentPermissionScope.user), 
	CR_DU(true, true, false, true, DocumentPermissionScope.user);

	private boolean canCreate = false;
	private boolean canRead = false;
	private boolean canUpdate = false;
	private boolean canDelete = false;
	private DocumentPermissionScope scope;

	/**
	 * Constructs a permission constant with explicit CRUD flags and a data scope.
	 *
	 * @param canCreate  {@code true} if create operations are permitted
	 * @param canRead    {@code true} if read operations are permitted
	 * @param canUpdate  {@code true} if update operations are permitted
	 * @param canDelete  {@code true} if delete operations are permitted
	 * @param scope      the data visibility scope; must not be {@code null}
	 */
	private DocumentPermission(boolean canCreate, 
								boolean canRead, 
								boolean canUpdate, 
								boolean canDelete, 
								DocumentPermissionScope scope) {
		this.canCreate = canCreate;
		this.canRead = canRead;
		this.canUpdate = canUpdate;
		this.canDelete = canDelete;
		this.scope = scope;
	}

	/**
	 * Merges this permission with another, returning the union of CRUD bits and the
	 * broader of the two data scopes.
	 *
	 * <p>The merge operation is commutative: {@code a.mergePermission(b)} and
	 * {@code b.mergePermission(a)} return the same permission constant.
	 *
	 * @param anotherPermission  the permission to merge with; must not be {@code null}
	 * @return the merged permission constant; never {@code null}
	 */
	public DocumentPermission mergePermission(DocumentPermission anotherPermission) {
		char[] permissions = {'_', '_', '_', '_', scope.charValue()};
		if (canCreate || anotherPermission.canCreate) {
			permissions[0] = 'C';
		}
		if (canRead || anotherPermission.canRead) {
			permissions[1] = 'R';
		}
		if (canUpdate || anotherPermission.canUpdate) {
			permissions[2] = 'U';
		}
		if (canDelete || anotherPermission.canDelete) {
			permissions[3] = 'D';
		}
		if (scope.compareTo(anotherPermission.scope) < 0) {
			permissions[4] = anotherPermission.scope.charValue();
		}
		String permission = new String(permissions);

		DocumentPermission result = this;
		if (! permission.equals(toString())) {
			result = DocumentPermission.valueOf(permission);
		}

		return result;
	}

	/**
	 * Returns whether create operations are permitted by this permission.
	 *
	 * @return {@code true} if create is permitted
	 */
	public boolean canCreate() {
		return canCreate;
	}

	/**
	 * Returns whether delete operations are permitted by this permission.
	 *
	 * @return {@code true} if delete is permitted
	 */
	public boolean canDelete() {
		return canDelete;
	}

	/**
	 * Returns whether read operations are permitted by this permission.
	 *
	 * @return {@code true} if read is permitted
	 */
	public boolean canRead() {
		return canRead;
	}

	/**
	 * Returns whether update operations are permitted by this permission.
	 *
	 * @return {@code true} if update is permitted
	 */
	public boolean canUpdate() {
		return canUpdate;
	}

	/**
	 * Returns the data visibility scope associated with this permission.
	 *
	 * @return the scope; never {@code null}
	 */
	public DocumentPermissionScope getScope() {
		return scope;
	}
}
