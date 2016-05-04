package org.skyve.metadata.user;

import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

/**
 * 
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
	 * 
	 * @param canCreate
	 * @param canRead
	 * @param canUpdate
	 * @param canDelete
	 * @param scope
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
	 * 
	 * @param anotherPermission
	 * @return
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
	 * 
	 * @return
	 */
	public boolean canCreate() {
		return canCreate;
	}

	/**
	 * 
	 * @return
	 */
	public boolean canDelete() {
		return canDelete;
	}

	/**
	 * 
	 * @return
	 */
	public boolean canRead() {
		return canRead;
	}

	/**
	 * 
	 * @return
	 */
	public boolean canUpdate() {
		return canUpdate;
	}

	/**
	 * 
	 * @return
	 */
	public DocumentPermissionScope getScope() {
		return scope;
	}
}
