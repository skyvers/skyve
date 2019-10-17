package modules.admin.UserDashboard;

import java.util.ArrayList;
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
import org.skyve.metadata.view.View.ViewType;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.Util;

import modules.admin.ModulesUtil;
import modules.admin.User.UserExtension;
import modules.admin.domain.Audit;
import modules.admin.domain.Audit.Operation;
import modules.admin.domain.UserDashboard;

public class UserDashboardExtension extends UserDashboard {

	private static final String DEFAULT_ICON_CLASS = "fa fa-file-o";
	private static final int TILE_CELL_PADDING = 2;
	private static final int TILE_COUNT_LIMIT = 6;
	private static final int TILE_TEXT_LIMIT = 50;
	private static final String TILE_NEW_COLOUR = "#82E0AA";
	private static final String TILE_UPDATE_COLOUR = "#AED6F1";
	private static final String TILE_VIEW_COLOUR = "#FDEBD0";
	private static final long serialVersionUID = -6841455574804123970L;
	private static List<String> tiles;

	@Override
	public String getFavourites() {
		return createFavourites();
	}

	/**
	 * Records most popularly updated by the filter user
	 * 
	 * @return
	 */
	public static List<Bean> popularUpdates(UserExtension filterUser){
		
		Persistence pers = CORE.getPersistence();
		DocumentQuery q = pers.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
		q.getFilter().addNotEquals(Audit.operationPropertyName, Operation.delete);
		if(filterUser!=null) {
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
	 * Records most recently updated by the filter user
	 * 
	 * @return
	 */
	public static List<Bean> recentUpdates(UserExtension filterUser){
		
		Persistence pers = CORE.getPersistence();
		DocumentQuery q = pers.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
		q.getFilter().addNotEquals(Audit.operationPropertyName, Operation.delete);
		if(filterUser!=null) {
			q.getFilter().addEquals(Audit.userNamePropertyName, filterUser.getUserName());
		}
		q.addBoundProjection(Audit.timestampPropertyName);
		q.addBoundProjection(Audit.auditModuleNamePropertyName);
		q.addBoundProjection(Audit.auditDocumentNamePropertyName);
		q.addBoundProjection(Audit.auditBizIdPropertyName);
		q.addBoundOrdering(Audit.timestampPropertyName, SortDirection.descending);
		q.addBoundOrdering(Audit.millisPropertyName, SortDirection.descending);
		q.setMaxResults(10);
		
		return q.projectedResults();
	}
	
	/**
	 * Documents most recently created by the filter user
	 * 
	 * @param filterUser
	 * @return
	 */
	public static List<Bean> recentInsertDocuments(UserExtension filterUser){
		Persistence pers = CORE.getPersistence();
		DocumentQuery q = pers.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
		q.getFilter().addEquals(Audit.operationPropertyName, Operation.insert);
		q.getFilter().addNotEquals(Audit.auditModuleNamePropertyName, Audit.MODULE_NAME);
		if(filterUser!=null) {
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
	 * Create markup for shortcut links to favourite actions for this user
	 * 
	 * @return
	 */
	private static String createFavourites() {

		Persistence pers = CORE.getPersistence();
		tiles = new ArrayList<>();
		UserExtension currentUser = ModulesUtil.currentAdminUser();

		//temporarily elevate user permissions to view Audit records
		pers.setDocumentPermissionScopes(DocumentPermissionScope.customer);

		// favourites for the most common record saved by me (which hasn't been deleted)
		if (tiles.size() < TILE_COUNT_LIMIT) {

			createTilesCommon(popularUpdates(currentUser), Operation.update, 1, "Popular by me");
		}

		// favourite for the most recent record saved by me (which hasn't been deleted)
		if (tiles.size() < TILE_COUNT_LIMIT) {

			createTilesRecent(recentUpdates(currentUser), Operation.update, 1, "Recent by me");
		}

		// favourite for the most common record saved by anyone (which hasn't been deleted)
		if (tiles.size() < TILE_COUNT_LIMIT) {


			createTilesCommon(popularUpdates(null), Operation.update, 1, "Popular by everyone");
		}

		if (tiles.size() < TILE_COUNT_LIMIT) {

			createTilesRecent(recentInsertDocuments(currentUser), Operation.insert,1, "Recently created");
			
		}
		
		if (tiles.size() < TILE_COUNT_LIMIT) {
			// add favourites to home documents for all modules the user has access to
			Customer customer = pers.getUser().getCustomer();
			for (Module module : customer.getModules()) {

				// check if user has access to the home document
				Document document = module.getDocument(customer, module.getHomeDocumentName());
				if (ViewType.list.equals(module.getHomeRef())) {
					if (CORE.getUser().canCreateDocument(document)) {
						addTile(createTile(Operation.insert, module.getName(), module.getHomeDocumentName(), null), "Suggested for creation");
					}
				} else {
					if (CORE.getUser().canAccessDocument(document)) {
						addTile(createTile(Operation.update, module.getName(), module.getHomeDocumentName(), null), "Suggested for viewing");
					}
				}
			}
		}

		pers.resetDocumentPermissionScopes();

		// render the tiles for display
		StringBuilder favourites = new StringBuilder();
		favourites.append("<table cellpadding=\"").append(TILE_CELL_PADDING).append("\">");
		for (String tile : tiles) {
			favourites.append("<tr><td>").append(tile).append("</td></tr>");
		}
		favourites.append("</table>");

		return favourites.toString();
	}

	/**
	 * Construct a list of tiles shortcuts to perform the operation on the audited beans
	 *  
	 * @param audits
	 * @param operation
	 * @param top
	 * @param reason
	 */
	public static void createTilesCommon(List<Bean> audits, Operation operation, int top, String reason) {
		
		int count = 0;
		for (Bean audit : audits) {
			String moduleName = (String) Binder.get(audit, Audit.auditModuleNamePropertyName);
			String documentName = (String) Binder.get(audit, Audit.auditDocumentNamePropertyName);
			Customer customer = CORE.getCustomer();
			Module module = customer.getModule(moduleName);
			Document document = module.getDocument(customer, documentName);
			
			if (CORE.getUser().canAccessDocument(document)) {
				String id = (String) Binder.get(audit, Audit.auditBizIdPropertyName);
				Bean exists = CORE.getPersistence().retrieve(moduleName, documentName, id);
				// Long score = (Long) Binder.get(b, "Score");
				if (exists != null) {
					boolean added = addTile(createTile(Operation.update, moduleName, documentName, exists), reason);
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

	/**
	 * When two actions happen at a similar timestamp, the latest will be the most senior
	 * 
	 * @param audits
	 * @param operation
	 */
	public static void createTilesRecent(List<Bean> audits, Operation operation, int top, String reason) {

		int count = 0;
		Set<String> documents = new HashSet<>();
		Timestamp lastTime = null;
		for (Bean audit : audits) {
			Timestamp timestamp = (Timestamp) Binder.get(audit, Audit.timestampPropertyName);
			String moduleName = (String) Binder.get(audit, Audit.auditModuleNamePropertyName);
			String documentName = (String) Binder.get(audit, Audit.auditDocumentNamePropertyName);
			if(Operation.update.equals(operation)) {
				String id = (String) Binder.get(audit, Audit.auditBizIdPropertyName);
				Bean exists = CORE.getPersistence().retrieve(moduleName, documentName, id);
				if (exists != null) {
					if ((lastTime == null || lastTime.before(timestamp))
							&& !documents.contains(documentName)) {
						boolean added = addTile(createTile(operation, moduleName, documentName, exists), reason);
						lastTime = timestamp;
						if (added) {
							count++;
						}
					}
				}
			} else {
				if ((lastTime == null || lastTime.before(timestamp))
						&& !documents.contains(documentName)) {
					boolean added = addTile(createTile(operation, moduleName, documentName, null), reason);
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

	/**
	 * Limit the number of tiles to display
	 * returns true if tile was added
	 * 
	 * @param tile
	 */
	private static boolean addTile(String tile, @SuppressWarnings("unused") String justification) {
		int size = tiles.size();
		if (tiles.size() < TILE_COUNT_LIMIT) {
			boolean found = false;
			for (String prev : tiles) {
				if (prev.equals(tile)) {
					found = true;
					break;
				}
			}
			if (!found) {
				tiles.add(tile);
			}
		}
		return size != tiles.size();
	}

	/**
	 * create a clickable tile markup for the action
	 * 
	 * @param moduleName
	 * @param documentName
	 * @param action
	 * @return
	 */
	private static String createTile(Operation operation, String moduleName, String documentName, Bean bean) {
		StringBuilder link = new StringBuilder();
		link.append(Util.getHomeUrl());
		link.append("?a=e&m=").append(moduleName).append("&d=").append(documentName);
		if (bean != null) {
			link.append("&i=").append(bean.getBizId());
		}

		User user = CORE.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(moduleName);
		Document document = module.getDocument(customer, documentName);

		String iconClass = (document.getIconStyleClass() == null ? DEFAULT_ICON_CLASS : document.getIconStyleClass());
		String singularAlias = document.getSingularAlias();
		String backgroundColour;
		String action;
		switch (operation) {
		case insert:
			action = "Create a new ";
			backgroundColour = TILE_NEW_COLOUR;
			break;
		case update:
			// check if the document is persistent for "view" or "edit"
			if (document.getPersistent() == null) {
				action = "View ";
				backgroundColour = TILE_VIEW_COLOUR;
			} else {
				action = operation.toDescription();
				backgroundColour = TILE_UPDATE_COLOUR;
			}
			break;
		default:
			action = operation.toDescription();
			backgroundColour = TILE_VIEW_COLOUR;
		}

		// div style
		String divStyle = "style=\"cursor: pointer; background-color: " + backgroundColour + "; opacity: 0.9; border: 1px solid grey; border-radius: 3px; padding: 3px 3px 3px 3px;\"";

		StringBuilder sb = new StringBuilder();
		sb.append("<div onclick=\"location.href='").append(link.toString()).append("';\"");
		sb.append(divStyle);
		//hover 
		sb.append(" onMouseOver=\"this.style.opacity=1.0\"");
		sb.append(" onMouseOut=\"this.style.opacity=0.9\"");
		sb.append(">");

		// tile is a table with an icon and text
		sb.append("<table cellpadding=\"").append(TILE_CELL_PADDING).append("\"><tr>");

		// icon
		sb.append("<td width=\"28px\" align=\"center\">");
		String icon = "<i class=\"" + iconClass + "\" style=\"font-size:24px;\"></i>";
		if (bean != null) {
			// provide a thumbnail for the first image attribute type
			for (Attribute a : document.getAttributes()) {
				if (AttributeType.image.equals(a.getAttributeType())) {
					String cId = (String) Binder.get(bean, a.getName());
					String imgSrc = "content?_n=" + cId + "&_doc=" + moduleName + "." + documentName + "&_b=" + a.getName() + "&_w=24&_h=24";
					icon = "<img src=\"" + imgSrc + "\" />";
					break;
				}
			}
		}
		sb.append(icon);
		sb.append("</td>");

		// link text
		sb.append("<td>");

		StringBuilder tileText = new StringBuilder();
		tileText.append(action).append(" ").append(singularAlias);
		if (bean != null && bean.getBizKey() != null) {
			tileText.append(" - ").append(bean.getBizKey());
		}
		sb.append((tileText.length() > TILE_TEXT_LIMIT ? tileText.toString().substring(0, TILE_TEXT_LIMIT - 3) + "..." : tileText));
		sb.append("</td>");

		// close tile
		sb.append("</tr></table>");
		sb.append("</div>");

		return sb.toString();
	}
	
}
