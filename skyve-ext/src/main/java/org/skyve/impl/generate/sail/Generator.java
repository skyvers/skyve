package org.skyve.impl.generate.sail;

import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;

import org.skyve.CORE;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.web.UserAgentType;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.module.query.DocumentQueryDefinition;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Interaction;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.interaction.TestDataEnter;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMenu;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.list.ListModel;

public class Generator {
/*
	public static final void main(String[] args) throws Exception {
		if (args.length != 1) {
			System.err.println("args are <customerName>");
			System.exit(1);
		}

		UtilImpl.APPS_JAR_DIRECTORY = "/Users/mike/dtf/skyve/skyve-ee/javaee/skyve.ear/apps.jar/";
		AbstractRepository.set(new LocalDesignRepository());
		Customer customer = AbstractRepository.get().getCustomer(customerName);

System.out.println(visitModules(args[0]));
	}
*/
	public static List<Automation> visitMenus(User user, String uxui, UserAgentType userAgentType)
	throws Exception {
		CORE.getRepository().resetMenus(user);
		UserImpl u = (UserImpl) user;
		Customer c = user.getCustomer();
		List<Automation> result = new ArrayList<>();
		
		// visit each module menu
		for (String moduleName : user.getAccessibleModuleNames()) {
			Module m = c.getModule(moduleName);
			Automation automation = new Automation();
			automation.setUserAgentType(userAgentType);
			automation.setUxui(uxui);
			List<Interaction> interactions = automation.getInteractions();
			Menu menu = u.getModuleMenu(moduleName);
			menu(c, m, null, menu.getItems(), uxui, interactions);

			// some modules may have no access at all for this user
			if (interactions.size() > 0) {
				result.add(automation);
			}
		}

//		return XMLMetaData.marshalSAIL(result);
		return result;
	}
	
	private static void menu(Customer c,
								Module m,
								String prefix,
								List<MenuItem> items,
								String uxui,
								List<Interaction> interactions) {
		String moduleName = m.getName();
		for (MenuItem item : items) {
			if (item.isApplicable(uxui)) {
				String path = item.getName();
				if (prefix != null) {
					path = String.format("%s/%s", prefix, path);
				}

				if (item instanceof MenuGroup) {
					menu(c, m, path, ((MenuGroup) item).getItems(), uxui, interactions);
				}
				else {
					Interaction interaction = new Interaction();
					interaction.setName("Menu " + path);
					List<Step> steps = interaction.getSteps();
					
					NavigateMenu navigate = new NavigateMenu();
					navigate.setModuleName(moduleName);
					navigate.setMenuPath(path);
					steps.add(navigate);
					
					if (item instanceof ListItem) {
						ListItem list = (ListItem) item;
						String queryName = list.getQueryName();
						String documentName = list.getDocumentName();
						String modelName = list.getModelName();

						Document d = null;
						
						if (queryName != null) {
							DocumentQueryDefinition query = m.getDocumentQuery(queryName);
							if (query == null) {
								query = m.getDocumentDefaultQuery(c, queryName);
							}
							d = query.getDocumentModule(c).getDocument(c, query.getDocumentName());
						}
						else {
							d = m.getDocument(c, documentName);
							if (modelName != null) {
								ListModel<?> model = CORE.getRepository().getListModel(c, d, modelName, true);
								d = model.getDrivingDocument();
							}
						}
						
						crud(d, steps);
					}
					else if (item instanceof EditItem) {
						Document d = m.getDocument(c, ((EditItem) item).getDocumentName());
						edit(c, m, d, uxui, steps);
					}

					interactions.add(interaction);
				}
			}
		}
	}
	
	public static List<Automation> visitModules(User u,
													String uxui,
													UserAgentType userAgentType)
	throws Exception {
		List<Automation> result = new ArrayList<>();
		
		Customer c = u.getCustomer();
		for (Module m : c.getModules()) {
			String moduleName = m.getName();
			Automation automation = new Automation();
			automation.setUxui(uxui);
			automation.setUserAgentType(userAgentType);
			List<Interaction> interactions = automation.getInteractions();
			
			for (Entry<String, DocumentRef> entry : m.getDocumentRefs().entrySet()) {
				DocumentRef documentRef = entry.getValue();
				if (documentRef.getOwningModuleName().equals(moduleName)) {
					String documentName = entry.getKey();
					Document d = m.getDocument(c, documentName);
					if ((! d.isAbstract()) && u.canAccessDocument(d)) {
						if (d.getPersistent() == null) {
							Interaction interaction = new Interaction();
							interaction.setName("Edit document " + documentName);
							List<Step> steps = interaction.getSteps();
							
							NavigateEdit navigate = new NavigateEdit();
							navigate.setModuleName(moduleName);
							navigate.setDocumentName(documentName);
							steps.add(navigate);

							edit(c, m, d, uxui, steps);
							
							interactions.add(interaction);
						}
						else {
							Interaction interaction = new Interaction();
							interaction.setName("CRUD document " + documentName);
							List<Step> steps = interaction.getSteps();

							NavigateList navigate = new NavigateList();
							navigate.setModuleName(moduleName);
							navigate.setDocumentName(documentName);
							steps.add(navigate);
							
							crud(d, steps);

							interactions.add(interaction);
						}
					}
				}
			}
			result.add(automation);
		}
		
		return result;
	}
	
	private static void edit(Customer c, Module m, Document d, String uxui, List<Step> steps) {
		TestDataEnter enter = new TestDataEnter();
		steps.add(enter);
		
		View createView = d.getView(uxui, c, ViewType.create.toString());
		View editView = d.getView(uxui, c, ViewType.edit.toString());
		new GenerateViewVisitor((CustomerImpl) c,
									(ModuleImpl) m,
									(DocumentImpl) d,
									(ViewImpl) ((createView != editView) ? createView : editView),
									steps).visit();
		
/*
		visit the view and determine the grids to data enter.
		Need to resolve whether to click tabs or not???
		What if tabs are replaced with accordions - how do I click them?
		But the tabSelect command wont work anyway!
		Could be busy clicking tabs.
*/
	}
	
	private static void crud(Document d, List<Step> steps) {
		// TODO implement
	}
}
