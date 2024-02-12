package org.skyve.impl.web.service.prime;

import java.io.IOException;
import java.io.PrintWriter;
import java.security.Principal;

import org.skyve.content.MimeType;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.LinkItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.module.menu.MenuRenderer;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

public class InitialiseServlet extends HttpServlet {
	private static final long serialVersionUID = -1850794772969374823L;

	@Override
	protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		doGet(req, resp);
	}

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
    	response.setContentType(MimeType.json.toString());
        response.setCharacterEncoding(Util.UTF8);
		response.addHeader("Cache-control", "private,no-cache,no-store"); // never
		response.addDateHeader("Expires", 0); // never
		
		StringBuilder json = new StringBuilder(2048);
		try {
			Principal userPrincipal = request.getUserPrincipal();
	    	User user = WebUtil.processUserPrincipalForRequest(request, (userPrincipal == null) ? null : userPrincipal.getName(), true);
			if (user == null) {
				throw new SessionEndedException(request.getLocale());
			}

			UxUi uxui = UserAgent.getUxUi(request);
			String chosenModuleName = Util.processStringValue(request.getParameter(AbstractWebContext.MODULE_NAME));
			
			addModuleMenu(uxui.getName(), user,  chosenModuleName, json);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			json.setLength(0);
			json.append("{\"menu\":[]}");
		}

		try (PrintWriter pw = response.getWriter()) {
			pw.append(json);
			pw.flush();
		}
	}
	
	private static void addModuleMenu(final String uxui, 
										final User user,
										final String chosenModuleName,
										final StringBuilder json) {
		json.append("[");
		
		new MenuRenderer(uxui, chosenModuleName) {
			private void appendItem(MenuItem item, String iconStyleClass, Module itemModule, String function, String ref) {
				json.append("{\"label\":\"").append(item.getLocalisedName());
				if (iconStyleClass != null) {
					json.append("\",\"icon\":\"").append(iconStyleClass);
				}
				json.append("\",\"command\":()=>{window.location='#/").append(itemModule.getName());
				json.append('/').append(function).append(ref).append("'}},");				
			}
			
			@Override
			public void renderModuleMenu(Menu menu, Module menuModule, boolean open) {
				json.append("{\"label\":\"").append(menuModule.getLocalisedTitle()).append("\",\"items\":[");
			}

			@Override
			public void renderMenuGroup(MenuGroup group, Module menuModule) {
				json.append("{\"label\":\"").append(group.getLocalisedName()).append("\",\"items\":[");
			}
			
			@Override
			public void renderCalendarItem(CalendarItem item,
											Module menuModule,
											Module itemModule,
											Document itemDocument,
											String itemQueryName,
											String icon16,
											String iconStyleClass) {
				String modelName = item.getModelName();
				appendItem(item, iconStyleClass, itemModule, (modelName == null) ? itemQueryName : modelName, "Cal");
			}
			
			@Override
			public void renderEditItem(EditItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String icon16,
										String iconStyleClass) {
				appendItem(item, iconStyleClass, itemModule, itemDocument.getName(), "");
			}

			@Override
			public void renderLinkItem(LinkItem item, Module menuModule, boolean relative, String absoluteHref) {
				json.append("{\"label\":\"").append(item.getLocalisedName());
				json.append("\",\"command\":()=>{window.location='").append(absoluteHref).append("'}},");
			}
			
			@Override
			public void renderListItem(ListItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String itemQueryName,
										String icon16,
										String iconStyleClass) {
				String modelName = item.getModelName();
				appendItem(item, iconStyleClass, itemModule, (modelName == null) ? itemQueryName : modelName, "List");
			}
			
			@Override
			public void renderMapItem(MapItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String itemQueryName,
										String icon16,
										String iconStyleClass) {
				String modelName = item.getModelName();
				appendItem(item, iconStyleClass, itemModule, (modelName == null) ? itemQueryName : modelName, "Map");
			}
			
			@Override
			public void renderTreeItem(TreeItem item,
										Module menuModule,
										Module itemModule,
										Document itemDocument,
										String itemQueryName,
										String icon16,
										String iconStyleClass) {
				String modelName = item.getModelName();
				appendItem(item, iconStyleClass, itemModule, (modelName == null) ? itemQueryName : modelName, "Tree");
			}
			
			@Override
			public void renderedMenuGroup(MenuGroup group, Module menuModule) {
				json.setLength(json.length() - 1); // remove trailing comma
				json.append("]},");
			}
			
			@Override
			public void renderedModuleMenu(Menu menu, Module menuModule, boolean open) {
				json.setLength(json.length() - 1); // remove trailing comma
				json.append("]},");
			}
		}.render(user);

        json.setLength(json.length() - 1);
        json.append("]");
	}
}
