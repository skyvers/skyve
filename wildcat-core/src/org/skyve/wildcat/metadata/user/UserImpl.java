package org.skyve.wildcat.metadata.user;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.user.DocumentPermission;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;
import org.skyve.wildcat.metadata.repository.AbstractRepository;
import org.skyve.wildcat.metadata.repository.module.ContentPermission;
import org.skyve.wildcat.metadata.repository.module.ContentRestriction;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.persistence.SQLImpl;
import org.skyve.wildcat.util.UtilImpl;

public class UserImpl implements User {
	/**
	 * For Serialization.
	 */
	private static final long serialVersionUID = -8485741818564437957L;

	protected static final String SUPER_ROLE = "design.BizHubDesigner";
	private static final String SECURITY_ADMINISTRATOR_ROLE = "admin.SecurityAdministrator";

	/**
	 * Represents a user that does not belong to a data group.
	 */
	public static final String DATA_ADMINISTRATOR_ROLE = "admin.DataAdministrator";

	private String id;
	private String name;
	private String contactId;
	private String contactName;
	private String customerName;
	private String dataGroupId;
	private String homeModuleName;

	/**
	 * User (session) attributes. Keep this small since the user is in the web session.
	 */
	private Map<String, Object> attributes = new TreeMap<>();

	/**
	 * To allow SuperUser to set SUPER Role
	 */
	protected Set<String> roleNames = new TreeSet<>();

	/**
	 * Document Name -> CRUD permission
	 */
	private Map<String, DocumentPermission> documentPermissions = new TreeMap<>();

	/**
	 * module.document.action
	 */
	private Set<String> actions = new TreeSet<>();

	/**
	 * Module name -> Menu
	 */
	private Map<String, Menu> moduleMenuMap = new TreeMap<>();

	/**
	 * module.document.attribute
	 */
	private Set<String> contentRestrictions = new TreeSet<>();

	/**
	 * module.document.attribute
	 */
	private Set<String> contentPermissions = new TreeSet<>();

	@Override
	public String getId() {
		return id;
	}

	@Override
	public void setId(String id) {
		this.id = id;
	}

	@Override
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	@Override
	public String getContactId() {
		return contactId;
	}

	public void setContactId(String contactId) {
		this.contactId = contactId;
	}

	@Override
	public String getContactName() {
		return contactName;
	}

	public void setContactName(String contactName) {
		this.contactName = contactName;
	}

	/**
	 * Hold other user (session) attributes for processing. 
	 * NOTE: Keep this small since the user is in the web session.
	 * 
	 * @return The attributes Map.
	 */
	@Override
	public Map<String, Object> getAttributes() {
		return attributes;
	}

	@Override
	public Customer getCustomer() throws MetaDataException {
		return AbstractRepository.get().getCustomer(customerName);
	}

	@Override
	public void setCustomerName(String customerName) {
		this.customerName = customerName;
	}

	@Override
	public String getCustomerName() {
		return customerName;
	}

	@Override
	public String getDataGroupId() {
		return dataGroupId;
	}

	@Override
	public void setDataGroupId(String dataGroupId) {
		this.dataGroupId = dataGroupId;
	}

	@Override
	public String getHomeModuleName() {
		return homeModuleName;
	}

	public void setHomeModuleName(String homeModuleName) {
		this.homeModuleName = homeModuleName;
	}

	public void addRole(RoleImpl role) {
		StringBuilder sb = new StringBuilder(64);

		sb.append(role.getOwningModule().getName()).append('.').append(role.getName());
		roleNames.add(sb.toString());
		
		for (Privilege privilege : role.getPrivileges()) {
			if (privilege instanceof DocumentPrivilege) {
				DocumentPermission permission = ((DocumentPrivilege) privilege).getPermission();
				putDocumentPermission(role.getOwningModule().getName(), privilege.getName(), permission);
			}
			else if (privilege instanceof ActionPrivilege) {
				// will add to set if not already present
				sb.setLength(0);
				sb.append(role.getOwningModule().getName()).append('.');
				sb.append(((ActionPrivilege) privilege).getDocumentName()).append('.');
				sb.append(privilege.getName());
				actions.add(sb.toString());
			}
		}

		for (ContentRestriction contentRestriction : role.getContentRestrictions()) {
			sb.setLength(0);
			sb.append(role.getOwningModule().getName()).append('.');
			sb.append(contentRestriction.getDocumentName()).append('.');
			sb.append(contentRestriction.getAttributeName());
			contentRestrictions.add(sb.toString());
		}

		for (ContentPermission contentPermission : role.getContentPermissions()) {
			sb.setLength(0);
			sb.append(role.getOwningModule().getName()).append('.');
			sb.append(contentPermission.getDocumentName()).append('.');
			sb.append(contentPermission.getAttributeName());
			contentPermissions.add(sb.toString());
		}
	}

	private void putDocumentPermission(String moduleName, String documentName, DocumentPermission documentPermission) {
		DocumentPermission mergedPermission = documentPermission;
		
		String fullyQualifiedDocumentName = new StringBuilder(32).append(moduleName).append('.').append(documentName).toString();
		DocumentPermission existingDocumentPermission = documentPermissions.get(fullyQualifiedDocumentName);
		if (existingDocumentPermission != null) {
			mergedPermission = existingDocumentPermission.mergePermission(mergedPermission);
		}

		documentPermissions.put(fullyQualifiedDocumentName, mergedPermission);
	}

	public Set<String> getFullyQualifiedDocumentNames() {
		return documentPermissions.keySet();
	}

	/**
	 * Gets the accessible menu structure for a user and module.
	 * 
	 * @param moduleName
	 * @return
	 */
	public Menu getModuleMenu(String moduleName) {
		return moduleMenuMap.get(moduleName);
	}

	public void putModuleMenu(String moduleName, Menu menu) {
		moduleMenuMap.put(moduleName, menu);
	}

	public void clearModuleMenus() {
		moduleMenuMap.clear();
	}

	@Override
	public Set<String> getAccessibleModuleNames() {
		Set<String> result = new TreeSet<>();

		// add module name if menu has accessible items
		for (Entry<String, Menu> entry : moduleMenuMap.entrySet()) {
			if (! entry.getValue().getItems().isEmpty()) {
				result.add(entry.getKey());
			}
		}

		return result;
	}

	@Override
	public boolean isInRole(String moduleName, String roleName) {
		String fullyQualifiedRoleName = new StringBuilder(32).append(moduleName).append('.').append(roleName).toString();
		return (roleNames.contains(fullyQualifiedRoleName) || // user has the role
					roleNames.contains(SUPER_ROLE) || // user has the SUPER role
		// looking for data administrator role,
		// and user has no data group and has security admin role
		(DATA_ADMINISTRATOR_ROLE.equals(fullyQualifiedRoleName) && 
			(dataGroupId == null) && 
			roleNames.contains(SECURITY_ADMINISTRATOR_ROLE)));
	}

	@Override
	public DocumentPermissionScope getScope(String moduleName, String documentName) {
		DocumentPermissionScope result = roleNames.contains(SUPER_ROLE) ? 
											DocumentPermissionScope.customer :
											DocumentPermissionScope.none;
		DocumentPermission permission = documentPermissions.get(moduleName + '.' + documentName);
		if (permission != null) {
			result = permission.getScope();
		}

		return result;
	}

	/**
	 * Determine if we can read the document bean given the document scope etc. 
	 * NB. Cannot select the bean from bizhub data store in this method, coz it may be transient.
	 * 
	 * @param beanBizId
	 * @param beanBizModule
	 * @param beanBizDocument
	 * @param beanBizCustomer
	 * @param beanBizDataGroupId
	 * @param beanBizUserId
	 * @return
	 * @throws MetaDataException
	 * @throws DomainException
	 */
	@Override
	@SuppressWarnings("incomplete-switch")
	public boolean canReadBean(String beanBizId,
								String beanBizModule,
								String beanBizDocument,
								String beanBizCustomer,
								String beanBizDataGroupId,
								String beanBizUserId) 
	throws MetaDataException, DomainException {
		boolean result = roleNames.contains(SUPER_ROLE);

		if (! result) {
			DocumentPermission permission = documentPermissions.get(beanBizModule + '.' + beanBizDocument);
			if (permission != null) {
				if (permission.canRead()) {
					switch (permission.getScope()) {
					case global:
						result = true;
						break;
					case customer:
						result = customerName.equals(beanBizCustomer);
						break;
					case dataGroup:
						result = customerName.equals(beanBizCustomer);
						if (result && (dataGroupId != null)) {
							result = dataGroupId.equals(beanBizDataGroupId);
						}
						break;
					case user:
						result = customerName.equals(beanBizCustomer);
						if (result && (dataGroupId != null)) {
							result = dataGroupId.equals(beanBizDataGroupId);
						}
						if (result) {
							result = id.equals(beanBizUserId);
						}
					}
					if ((! result) && UtilImpl.SECURITY_TRACE) {
						StringBuilder trace = new StringBuilder(64);
						trace.append("Security - ");
						trace.append(beanBizModule).append('.');
						trace.append(beanBizDocument).append('.');
						trace.append(beanBizId).append(" denied - not in scope");
						UtilImpl.LOGGER.info(trace.toString());
					}
				}
				else {
					result = false;
					if (UtilImpl.SECURITY_TRACE) {
						StringBuilder trace = new StringBuilder(64);
						trace.append("Security - ");
						trace.append(beanBizModule).append('.');
						trace.append(beanBizDocument).append('.');
						trace.append(beanBizId).append(" denied - no read permission");
						UtilImpl.LOGGER.info(trace.toString());
					}
				}
			}
			else { // no permission defined
				// if document is a child document, get the parent
				// if the parent bean can be read then allow
				Customer customer = getCustomer();
				Module module = customer.getModule(beanBizModule);
				Document document = module.getDocument(customer, beanBizDocument);
				Document parentDocument = document.getParentDocument(customer);
				if (parentDocument == null) // document has no parent
				{
					result = false;
					if (UtilImpl.SECURITY_TRACE) {
						StringBuilder trace = new StringBuilder(64);
						trace.append("Security - ");
						trace.append(beanBizModule).append('.');
						trace.append(beanBizDocument).append('.');
						trace.append(beanBizId).append(" denied - no permission");
						UtilImpl.LOGGER.info(trace.toString());
					}
				}
				else // found a parent document
				{
					StringBuilder sb = new StringBuilder(256);
					sb.append("select p.").append(Bean.DOCUMENT_ID).append(", p.");
					sb.append(Bean.CUSTOMER_NAME).append(", p.");
					sb.append(Bean.DATA_GROUP_ID).append(", p.");
					sb.append(Bean.USER_ID);
					sb.append(" from ");
					sb.append(parentDocument.getPersistent().getPersistentIdentifier());
					sb.append(" as p inner join ");
					sb.append(document.getPersistent().getPersistentIdentifier());
					sb.append(" as c on p.").append(Bean.DOCUMENT_ID);
					sb.append(" = c.").append(ChildBean.PARENT_NAME);
					sb.append("_id where c.").append(Bean.DOCUMENT_ID).append(" = :");
					sb.append(Bean.DOCUMENT_ID);
					SQLImpl sql = new SQLImpl(sb.toString());
					sql.putParameter(Bean.DOCUMENT_ID, beanBizId);
					List<Object> rows = AbstractPersistence.get().retrieveInsecureSQL(sql);
					if (rows.isEmpty()) // bean is still transient - user hasn't saved
					{
						result = true;
					}
					else {
						Object[] values = (Object[]) rows.get(0);

						// deny if user can't read parent document
						result = canReadBean((String) values[0], 
												parentDocument.getOwningModuleName(), 
												parentDocument.getName(),
												(String) values[1], 
												(String) values[2], 
												(String) values[3]);
						if ((! result) && (UtilImpl.SECURITY_TRACE)) {
							StringBuilder trace = new StringBuilder(64);
							trace.append("Security - ");
							trace.append(beanBizModule).append('.');
							trace.append(beanBizDocument).append('.');
							trace.append(beanBizId).append(" denied - no read on parent");
							UtilImpl.LOGGER.info(trace.toString());
						}
					}
				}
			}
		}

		return result;
	}

	/**
	 * Indicates if the user is able to search or view the content
	 * 
	 * @return <code>true</code> if access is allowed, otherwise <code>false</code>.
	 * @throws DomainException
	 * @throws MetaDataException
	 */
	@Override
	public boolean canAccessContent(String bizId,
										String bizModule,
										String bizDocument,
										String bizCustomer,
										String bizDataGroupId,
										String bizUserId,
										String attributeName) 
	throws MetaDataException, DomainException {
		boolean result = roleNames.contains(SUPER_ROLE);

		if (! result) {
			String modocAndAttr = bizModule + '.' + bizDocument + '.' + attributeName;
			result = (! contentRestrictions.contains(modocAndAttr));
			if (result) {
				result = (contentPermissions.contains(modocAndAttr));
				if (! result) {
					// deny if user cant read parent document
					result = canReadBean(bizId, bizModule, bizDocument, bizCustomer, bizDataGroupId, bizUserId);
					if ((! result) && (UtilImpl.SECURITY_TRACE)) {
						StringBuilder trace = new StringBuilder(64);
						trace.append("Security - ");
						trace.append(bizModule).append('.');
						trace.append(bizDocument).append('.');
						trace.append(bizId).append(" denied - no read");
						UtilImpl.LOGGER.info(trace.toString());
					}
				}
			}
			else {
				if (UtilImpl.SECURITY_TRACE) {
					StringBuilder trace = new StringBuilder(64);
					trace.append("Security - ");
					trace.append(bizModule).append('.');
					trace.append(bizDocument).append('.');
					trace.append(attributeName).append(" denied - restricted content");
					UtilImpl.LOGGER.info(trace.toString());
				}
			}
		}

		return result;
	}

	@Override
	public boolean canAccessDocument(Document document) {
		boolean result = roleNames.contains(SUPER_ROLE);

		if (! result) {
			String modoc = new StringBuilder(64).append(document.getOwningModuleName()).append('.').append(document.getName()).toString();
			result = (documentPermissions.get(modoc) != null);
		}

		return result;
	}

	@Override
	public boolean canCreateDocument(Document document) {
		boolean result = roleNames.contains(SUPER_ROLE);

		if (! result) {
			String modoc = new StringBuilder(64).append(document.getOwningModuleName()).append('.').append(document.getName()).toString();
			DocumentPermission permission = documentPermissions.get(modoc);
			if (permission != null) {
				result = permission.canCreate();
			}
		}

		return result;
	}

	@Override
	public boolean canReadDocument(Document document) {
		boolean result = roleNames.contains(SUPER_ROLE);

		if (! result) {
			String modoc = new StringBuilder(64).append(document.getOwningModuleName()).append('.').append(document.getName()).toString();
			DocumentPermission permission = documentPermissions.get(modoc);
			if (permission != null) {
				result = permission.canRead();
			}
		}

		return result;
	}

	@Override
	public boolean canUpdateDocument(Document document) {
		boolean result = roleNames.contains(SUPER_ROLE);

		if (! result) {
			String modoc = new StringBuilder(64).append(document.getOwningModuleName()).append('.').append(document.getName()).toString();
			DocumentPermission permission = documentPermissions.get(modoc);
			if (permission != null) {
				result = permission.canUpdate();
			}
		}

		return result;
	}

	@Override
	public boolean canDeleteDocument(Document document) {
		boolean result = roleNames.contains(SUPER_ROLE);

		if (! result) {
			String modoc = new StringBuilder(64).append(document.getOwningModuleName()).append('.').append(document.getName()).toString();
			DocumentPermission permission = documentPermissions.get(modoc);
			if (permission != null) {
				result = permission.canDelete();
			}
		}

		return result;
	}

	@Override
	public boolean canExecuteAction(Document document, String actionName) {
		StringBuilder sb = new StringBuilder(128);
		sb.append(document.getOwningModuleName()).append('.').append(document.getName());
		String fullyQualifiedActionName = sb.append('.').append(actionName).toString();
		return (roleNames.contains(SUPER_ROLE) || actions.contains(fullyQualifiedActionName));
	}

	/**
	 * Return the client representation of the logged in user.
	 * 
	 * @return A new ClientUserData
	 */
	public ClientUserData createClientUser() {
		ClientUserData result = new ClientUserData();

		result.setId(id);
		result.setName(name);
		result.setContactId(contactId);
		result.setContactName(contactName);
		result.setCustomerName(customerName);
		result.setDataGroupId(dataGroupId);
		result.setRoleNames(roleNames);

		Map<String, Map<String, Boolean>> newPermissions = new TreeMap<>();
		for (String documentName : documentPermissions.keySet()) {
			DocumentPermission documentPermission = documentPermissions.get(documentName);
			Map<String, Boolean> permission = new TreeMap<>();
			permission.put("create", documentPermission.canCreate() ? Boolean.TRUE : Boolean.FALSE);
			permission.put("delete", documentPermission.canDelete() ? Boolean.TRUE : Boolean.FALSE);
			permission.put("read", documentPermission.canRead() ? Boolean.TRUE : Boolean.FALSE);
			permission.put("update", documentPermission.canUpdate() ? Boolean.TRUE : Boolean.FALSE);

			newPermissions.put(documentName, permission);
		}
		result.setDocumentPermissions(newPermissions);
		result.setActions(actions);
		result.setModuleMenuMap(moduleMenuMap);

		return result;
	}
}
