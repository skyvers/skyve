package modules.admin.UserDashboard;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.OWASP;
import org.skyve.util.Util;

import jakarta.inject.Inject;
import modules.admin.ModulesUtil;
import modules.admin.User.UserExtension;
import modules.admin.domain.Audit;
import modules.admin.domain.Audit.Operation;
import modules.admin.domain.Generic;
import modules.admin.domain.Job;
import modules.admin.domain.UserDashboard;

public class UserDashboardExtension extends UserDashboard {

	private static final long serialVersionUID = -6841455574804123970L;

	private static final String DEFAULT_ICON_CLASS = "fa-regular fa-file";
	private static final int TILE_COUNT_LIMIT = 6;
	
	private final Set<Tile> tiles = new HashSet<>();

	// used for 14 day dashboard calculations
	public static final Long TWO_WEEKS_AGO = Long.valueOf(System.currentTimeMillis() - 1209600000L);

	/**
	 * Returns true if the current logged in user has access to the Jobs document.
	 * 
	 * @return true if the jobs list should be visible in the User Dashboard
	 */
	@SuppressWarnings("static-method")
	public boolean canReadJobs() {
		Module module = CORE.getCustomer().getModule(Job.MODULE_NAME);
		Document document = module.getDocument(CORE.getCustomer(), Job.DOCUMENT_NAME);

		return CORE.getUser().canReadDocument(document);
	}

	@Inject
	private transient Persistence persistence;

	@Override
	public List<Generic> getFavourites() {
		super.getFavourites().clear();
		createFavourites();

		return super.getFavourites();
	}

	/**
	 * Create markup for shortcut links to favourite actions for this user
	 * 
	 * @return The HTML markup for the favourites
	 */
	private void createFavourites() {
		UserExtension currentUser = ModulesUtil.currentAdminUser();

		// temporarily elevate user permissions to view Audit records
		persistence.withDocumentPermissionScopes(DocumentPermissionScope.customer, p -> {
			// favourites for the most common record saved by me (which hasn't been deleted)
			if (tiles.size() < TILE_COUNT_LIMIT) {
				createTilesCommon(popularUpdates(currentUser), Operation.update, 1, "Popular by me");
			}

			// favourite for the most recent record saved by me (which hasn't been deleted)
			if (tiles.size() < TILE_COUNT_LIMIT) {
				createTilesRecent(recentUpdates(currentUser), Operation.update, 1, "Recent by me");
			}

			if (tiles.size() < TILE_COUNT_LIMIT) {
				createTilesRecent(recentInsertDocuments(currentUser), Operation.insert, 1, "Recently created");
			}

			if (tiles.size() < TILE_COUNT_LIMIT) {
				// add favourites to home documents for all modules the user has access to
				Customer customer = p.getUser().getCustomer();
				for (Module module : customer.getModules()) {
					// check if user has access to the home document
					Document document = module.getDocument(customer, module.getHomeDocumentName());
					if (ViewType.list.equals(module.getHomeRef())) {
						if (CORE.getUser().canCreateDocument(document)) {
							String reason = "Suggested for creation";
							addTile(createTile(Operation.insert, module.getName(), module.getHomeDocumentName(), null,
									reason));
						}
					} else {
						// exclude user dashboard - we are already here
						if (!UserDashboard.DOCUMENT_NAME.equals(document.getName()) && CORE.getUser().canAccessDocument(document)) {
							String reason = "Suggested for viewing";
							addTile(createTile(Operation.update, module.getName(), module.getHomeDocumentName(), null,
									reason));
						}
					}
				}
			}
		});

		// render the tiles for display
		for (Tile tile : tiles) {
			Generic g = Generic.newInstance();
			g.setMarkup1(tile.toMarkup());
			super.getFavourites().add(g);
		}
	}

	/**
	 * Records most popularly updated by the filter user
	 * 
	 * @return
	 */
	private List<Bean> popularUpdates(UserExtension filterUser) {

		DocumentQuery q = persistence.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
		q.getFilter().addGreaterThan(Audit.millisPropertyName, TWO_WEEKS_AGO);
		q.getFilter().addNotEquals(Audit.operationPropertyName, Operation.delete);
		if (filterUser != null) {
			q.getFilter().addEquals(Audit.userNamePropertyName, filterUser.getUserName());
		}
		q.addBoundGrouping(Audit.auditModuleNamePropertyName);
		q.addBoundGrouping(Audit.auditDocumentNamePropertyName);
		q.addBoundGrouping(Audit.auditBizIdPropertyName);

		q.addBoundProjection(Audit.auditModuleNamePropertyName);
		q.addBoundProjection(Audit.auditDocumentNamePropertyName);
		q.addBoundProjection(Audit.auditBizIdPropertyName);
		q.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "Score");

		q.addExpressionOrdering("4", SortDirection.descending); // sort by the 4th column
		q.setMaxResults(10);

		return q.projectedResults();
	}

	/**
	 * Documents most recently created by the filter user
	 * 
	 * @param filterUser
	 * @return
	 */
	private List<Bean> recentInsertDocuments(UserExtension filterUser) {
		DocumentQuery q = persistence.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
		q.getFilter().addGreaterThan(Audit.millisPropertyName, TWO_WEEKS_AGO);
		q.getFilter().addEquals(Audit.operationPropertyName, Operation.insert);
		q.getFilter().addNotEquals(Audit.auditModuleNamePropertyName, Audit.MODULE_NAME);
		if (filterUser != null) {
			q.getFilter().addEquals(Audit.userNamePropertyName, filterUser.getUserName());
		}
		q.addBoundProjection(Audit.timestampPropertyName);
		q.addBoundProjection(Audit.auditModuleNamePropertyName);
		q.addBoundProjection(Audit.auditDocumentNamePropertyName);
		q.addBoundOrdering(Audit.timestampPropertyName, SortDirection.descending);
		q.setMaxResults(10);

		return q.projectedResults();
	}

	/**
	 * Construct a list of tile shortcuts to perform the operation on the audited beans
	 * 
	 * @param audits
	 * @param operation
	 * @param top
	 * @param reason
	 */
	private void createTilesCommon(List<Bean> audits, Operation operation, int top, String reason) {

		try {
			int count = 0;
			for (Bean audit : audits) {
				String moduleName = (String) Binder.get(audit, Audit.auditModuleNamePropertyName);
				String documentName = (String) Binder.get(audit, Audit.auditDocumentNamePropertyName);
				Customer customer = CORE.getCustomer();
				Module module = customer.getModule(moduleName);
				Document document = module.getDocument(customer, documentName);

				if (CORE.getUser().canAccessDocument(document)) {
					String id = (String) Binder.get(audit, Audit.auditBizIdPropertyName);
					if (id != null) {
						Bean exists = persistence.retrieve(moduleName, documentName, id);
						if (exists != null) {
							boolean added = addTile(createTile(Operation.update, moduleName, documentName, exists, reason));
							if (added) {
								count++;
							}
						}
					}
				}
				if (count == top) {
					break;
				}
			}
		} catch (@SuppressWarnings("unused") Exception e) {
			// TODO: handle exception
			Util.LOGGER.warning("Failed to create " + reason + " tile.");
		}
	}

	/**
	 * When two actions happen at a similar timestamp, the latest will be the most senior
	 * 
	 * @param audits
	 * @param operation
	 */
	private void createTilesRecent(List<Bean> audits, Operation operation, int top, String reason) {

		int count = 0;
		Set<String> documents = new HashSet<>();
		Timestamp lastTime = null;
		for (Bean audit : audits) {
			Timestamp timestamp = (Timestamp) Binder.get(audit, Audit.timestampPropertyName);
			String moduleName = (String) Binder.get(audit, Audit.auditModuleNamePropertyName);
			String documentName = (String) Binder.get(audit, Audit.auditDocumentNamePropertyName);
			if (checkModuleDocumentCanBeRead(moduleName, documentName)) {
				if (Operation.update.equals(operation)) {
					String id = (String) Binder.get(audit, Audit.auditBizIdPropertyName);
					Bean exists = persistence.retrieve(moduleName, documentName, id);
					if (exists != null) {
						if ((lastTime == null || lastTime.before(timestamp))
								&& !documents.contains(documentName)) {
							boolean added = addTile(createTile(operation, moduleName, documentName, exists, reason));
							lastTime = timestamp;
							if (added) {
								count++;
							}
						}
					}
				} else {
					if ((lastTime == null || lastTime.before(timestamp))
							&& !documents.contains(documentName)) {
						boolean added = addTile(createTile(operation, moduleName, documentName, null, reason));
						lastTime = timestamp;
						if (added) {
							count++;
						}
					}
				}
				if (count == top) {
					break;
				}
			}
		}
	}

	/**
	 * Adds a tile to the working set of tiles if it hasn't already been
	 * added, and the tile limit has not already been reached.
	 * 
	 * @param tile The {@link Tile} to add
	 * @return True if a tile was added
	 */
	private boolean addTile(final Tile tile) {
		if (tile == null) {
			return false;
		}
		int size = tiles.size();
		if (tiles.size() < TILE_COUNT_LIMIT) {
			tiles.add(tile);
		}

		return size != tiles.size();
	}

	/**
	 * create a clickable tile markup for the action
	 * 
	 * @param moduleName
	 * @param documentName
	 * @param reason
	 * @param action
	 * @return
	 */
	private static Tile createTile(Operation operation, String moduleName, String documentName, Bean bean, String reason) {

		if (!checkModuleDocumentCanBeRead(moduleName, documentName)) {
			return null;
		}

		if (bean != null
				&& !CORE.getUser().canReadBean(bean.getBizId(), bean.getBizModule(), bean.getBizDocument(), bean.getBizCustomer(),
						bean.getBizDataGroupId(), bean.getBizUserId())) {
			return null;
		}

		StringBuilder link = new StringBuilder();
		link.append(Util.getBaseUrl());
		link.append("?a=e&m=").append(moduleName).append("&d=").append(documentName);
		if (bean != null) {
			link.append("&i=").append(bean.getBizId());
		}

		User user = CORE.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(moduleName);
		Document document = module.getDocument(customer, documentName);

		String action;
		String actionClass = null;
		String iconClass = (document.getIconStyleClass() == null ? DEFAULT_ICON_CLASS : document.getIconStyleClass());
		String singularAlias = document.getLocalisedSingularAlias();

		Tile.Operation tileOperation = Tile.Operation.view;

		switch (operation) {
			case delete:
				action = "Delete ";
				actionClass = "fa-times";
				tileOperation = Tile.Operation.delete;

				// clear the link if the user does not have delete permission
				if (!user.canDeleteDocument(document)) {
					link.setLength(0);
				}
				break;
			case insert:
				action = "Create a new ";
				actionClass = "fa-plus";
				tileOperation = Tile.Operation.insert;

				// clear the link if the user does not have create permission
				if (!user.canCreateDocument(document)) {
					link.setLength(0);
				}
				break;
			case update:
				// check if the document is persistent for "view" or "edit"
				if (document.getPersistent() == null) {
					action = "View ";
					actionClass = "fa-chevron-right";

					// clear the link if the user does not have read permission
					if (!user.canReadDocument(document)) {
						link.setLength(0);
					}
				} else {
					action = operation.toLocalisedDescription();
					actionClass = "fa-angle-up";
					tileOperation = Tile.Operation.update;

					// clear the link if the user does not have update permission
					if (!user.canUpdateDocument(document)) {
						link.setLength(0);
					}
				}
				break;
			default:
				action = operation.toLocalisedDescription();
				actionClass = "fa-chevron-right";

				// clear the link if the user does not have read permission
				if (!user.canReadDocument(document)) {
					link.setLength(0);
				}
		}

		// set the document icon
		String icon = String.format(""
				+ "<span class='icon'>"
				+ "  <i class='%1$s' style=\"font-size:24px;\"></i>"
				+ "</span>", iconClass);

		if (bean != null) {
			// provide a thumbnail for the first image or content attribute type
			for (Attribute a : document.getAllAttributes(customer)) {
				if (AttributeType.content.equals(a.getAttributeType())
						|| AttributeType.image.equals(a.getAttributeType())) {
					String cId = (String) Binder.get(bean, a.getName());
					if (cId != null) {
						String imgSrc = "content?_n=" + cId + "&_doc=" + moduleName + "." + documentName + "&_b=" + a.getName()
								+ "&_w=24&_h=24";
						icon = String.format("<span class='icon'>"
								+ "  <img src='%1$s'/>"
								+ "</span>", imgSrc);
					}
					break;
				}
			}
		}

		StringBuilder tileText = new StringBuilder();
		tileText.append(action).append(" ").append(singularAlias);
		if (bean != null && bean.getBizKey() != null) {
			tileText.append(" - ").append(OWASP.sanitise(Sanitisation.relaxed, bean.getBizKey()));
		}

		Tile tile = new Tile.Builder().action(action)
				.actionClass(actionClass)
				.icon(icon)
				.link(link.toString())
				.operation(tileOperation)
				.reason(reason)
				.title(tileText.toString())
				.build();

		return tile;
	}

	/**
	 * Since we are generating favourites from the audit history, it could be the case that:
	 * - the referenced module no longer exists, or can no longer be accessed by the user
	 * - the referenced document no longer exists, or can no longer be accessed by the user
	 * 
	 * @param moduleName
	 * @param documentName
	 * @return
	 */
	private static boolean checkModuleDocumentCanBeRead(String moduleName, String documentName) {
		Customer customer = CORE.getCustomer();
		Module module = null;
		Document document = null;
		boolean found = false;
		for (Module modl : customer.getModules()) {
			if (modl.getName().equals(moduleName)) {
				for (String docName : modl.getDocumentRefs().keySet()) {
					if (docName.equals(documentName)) {
						module = customer.getModule(moduleName);
						document = module.getDocument(customer, documentName);
						found = CORE.getUser().canReadDocument(document);
						break;
					}
				}
				break;
			}
		}
		return found;
	}

	/**
	 * Queries the 20 most recently updated audit records, filtered by the specified user if provided.
	 * 
	 * @param The user to filter the audits by
	 * @return The last 20 audits in the system
	 */
	private List<Bean> recentUpdates(UserExtension filterUser) {

		DocumentQuery q = persistence.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
		q.getFilter().addGreaterThan(Audit.millisPropertyName, TWO_WEEKS_AGO);
		q.getFilter().addNotEquals(Audit.operationPropertyName, Operation.delete);
		if (filterUser != null) {
			q.getFilter().addEquals(Audit.userNamePropertyName, filterUser.getUserName());
		}
		q.addBoundProjection(Audit.timestampPropertyName);
		q.addBoundProjection(Audit.auditModuleNamePropertyName);
		q.addBoundProjection(Audit.auditDocumentNamePropertyName);
		q.addBoundProjection(Audit.auditBizIdPropertyName);
		q.addBoundOrdering(Audit.timestampPropertyName, SortDirection.descending);
		q.addBoundOrdering(Audit.millisPropertyName, SortDirection.descending);
		q.setMaxResults(20);

		return q.projectedResults();
	}
}
