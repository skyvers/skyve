package org.skyve.impl.generate.sail;

import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;

import org.skyve.CORE;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.user.UserImpl;
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
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.sail.language.Interaction;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.interaction.TestDataEnter;
import org.skyve.metadata.sail.language.step.interaction.actions.Delete;
import org.skyve.metadata.sail.language.step.interaction.actions.Save;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridNew;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.metadata.user.User;
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
	public static List<Automation> visitMenus(User user,
												String uxui,
												UserAgentType userAgentType,
												TestStrategy testStrategy)
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
			automation.setTestStrategy(testStrategy);
			List<Interaction> interactions = automation.getInteractions();
			Menu menu = u.getModuleMenu(moduleName);
			menu(u, c, m, null, menu.getItems(), uxui, interactions);

			// some modules may have no access at all for this user
			if (! interactions.isEmpty()) {
				result.add(automation);
			}
		}

		return result;
	}

	public static Automation visitMenu(User user,
										String moduleName,
										String uxui,
										UserAgentType userAgentType,
										TestStrategy testStrategy)
	throws Exception {
		CORE.getRepository().resetMenus(user);
		UserImpl u = (UserImpl) user;
		Customer c = user.getCustomer();
		
		Module m = c.getModule(moduleName);
		Automation result = new Automation();
		result.setUserAgentType(userAgentType);
		result.setUxui(uxui);
		result.setTestStrategy(testStrategy);
		List<Interaction> interactions = result.getInteractions();
		Menu menu = u.getModuleMenu(moduleName);
		menu(u, c, m, null, menu.getItems(), uxui, interactions);

		return result;
	}

	private static void menu(User u,
								Customer c,
								Module m,
								String descriptionPrefix,
								List<MenuItem> items,
								String uxui,
								List<Interaction> interactions) {
		String moduleName = m.getName();
		for (MenuItem item : items) {
			if (item.isApplicable(uxui)) {
				String description = item.getName();
				if (descriptionPrefix != null) {
					description = String.format("%s::%s", descriptionPrefix, description);
				}

				if (item instanceof MenuGroup) {
					menu(u, c, m, description, ((MenuGroup) item).getItems(), uxui, interactions);
				}
				else {
					Interaction interaction = new Interaction();
					interaction.setName("Menu " + description);
					List<Step> steps = interaction.getSteps();
					
					if (item instanceof ListItem) {
						ListItem list = (ListItem) item;
						String queryName = list.getQueryName();
						String documentName = list.getDocumentName();
						String modelName = list.getModelName();

						NavigateList navigate = new NavigateList();
						navigate.setModuleName(moduleName);
						steps.add(navigate);

						Document d = null;
						
						if (queryName != null) {
							DocumentQueryDefinition query = m.getDocumentQuery(queryName);
							if (query == null) {
								navigate.setDocumentName(queryName);
								query = m.getDocumentDefaultQuery(c, queryName);
							}
							else {
								navigate.setQueryName(queryName);
							}
							d = query.getDocumentModule(c).getDocument(c, query.getDocumentName());
						}
						else {
							navigate.setDocumentName(documentName);
							d = m.getDocument(c, documentName);
							if (modelName != null) {
								navigate.setModelName(modelName);
								ListModel<?> model = CORE.getRepository().getListModel(c, d, modelName, true);
								d = model.getDrivingDocument();
							}
						}
						
						crud(u, c, m, d, uxui, navigate, steps);
					}
					else if (item instanceof EditItem) {
						NavigateEdit navigate = new NavigateEdit();
						navigate.setModuleName(moduleName);
						String documentName = ((EditItem) item).getDocumentName();
						navigate.setDocumentName(documentName);
						steps.add(navigate);

						Document d = m.getDocument(c, documentName);
						edit(c, m, d, uxui, steps);
					}

					interactions.add(interaction);
				}
			}
		}
	}
	
	public static List<Automation> visitModules(User u,
													String uxui,
													UserAgentType userAgentType,
													TestStrategy testStrategy)
	throws Exception {
		List<Automation> result = new ArrayList<>();
		
		Customer c = u.getCustomer();
		for (Module m : c.getModules()) {
			String moduleName = m.getName();
			// Don't process the test module as its cyclic and not required anyway
			if ("test".equals(moduleName)) {
				continue;
			}
			Automation automation = visitModule(u, moduleName, uxui, userAgentType, testStrategy);
			if (! automation.getInteractions().isEmpty()) {
				result.add(automation);
			}
		}
		
		return result;
	}

	public static Automation visitModule(User u,
											String moduleName,
											String uxui,
											UserAgentType userAgentType,
											TestStrategy testStrategy)
	throws Exception {
		Customer c = u.getCustomer();
		Module m = c.getModule(moduleName);
		
		Automation result = new Automation();
		result.setUxui(uxui);
		result.setUserAgentType(userAgentType);
		result.setTestStrategy(testStrategy);
		List<Interaction> interactions = result.getInteractions();
		
		for (Entry<String, DocumentRef> entry : m.getDocumentRefs().entrySet()) {
			DocumentRef documentRef = entry.getValue();
			if (documentRef.getOwningModuleName().equals(moduleName)) {
				String documentName = entry.getKey();
				Document d = m.getDocument(c, documentName);
				if ((! d.isAbstract()) && 
						(d.getParentDocumentName() == null) && 
						u.canAccessDocument(d)) {
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
						
						crud(u, c, m, d, uxui, navigate, steps);

						interactions.add(interaction);
					}
				}
			}
		}
		
		return result;
	}

	private static void edit(Customer c, Module m, Document d, String uxui, List<Step> steps) {
		GenerateViewVisitor visitor = new GenerateViewVisitor(c, m, d, uxui);
		visitor.visit();
		steps.addAll(visitor.getPopulateSteps());
		if (visitor.getHasSave()) {
			steps.add(new Save());
			steps.add(new TestDataEnter());
			steps.add(new Save());
		}
		steps.addAll(visitor.getActionSteps());
		if (visitor.getHasSave()) {
			steps.add(new Save());
		}
	}
	
	private static void crud(User u, Customer c, Module m, Document d, String uxui, NavigateList list, List<Step> steps) {
//System.out.println(String.format("CRUD %s.%s", m.getName(), d.getName()));
		if (u.canCreateDocument(d)) {
			ListGridNew nu = new ListGridNew();
			nu.setModuleName(list.getModuleName());
			nu.setDocumentName(list.getDocumentName());
			nu.setQueryName(list.getQueryName());
			nu.setModelName(list.getModelName());
			steps.add(nu);
			
			GenerateViewVisitor visitor = new GenerateViewVisitor(c, m, d, uxui);
			visitor.visit();
			steps.addAll(visitor.getPopulateSteps());
			if (visitor.getHasSave()) {
				steps.add(new Save());
				steps.add(new TestDataEnter());
				steps.add(new Save());
			}
			steps.addAll(visitor.getActionSteps());
			if (visitor.getHasDelete()) {
				steps.add(new Delete());
			}
		}
	}
}
