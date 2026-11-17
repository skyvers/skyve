package org.skyve.impl.generate.sail;

import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;

import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.sail.language.Interaction;
import org.skyve.metadata.sail.language.Procedure;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.interaction.TestDataEnter;
import org.skyve.metadata.sail.language.step.interaction.actions.Delete;
import org.skyve.metadata.sail.language.step.interaction.actions.Save;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridNew;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.metadata.sail.language.step.interaction.session.Login;
import org.skyve.metadata.sail.language.step.interaction.session.Logout;
import org.skyve.metadata.sail.language.step.interaction.session.SwitchMode;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.web.UserAgentType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Entry point for SAIL test-script generation from Skyve module/view metadata.
 *
 * <p>Walks a module's documents and their views, instantiating
 * {@link GenerateViewVisitor} for each view, and writes the resulting SAIL
 * scripts to the configured output directory.
 */
public class Generator {
/*
	public static final void main(String[] args) throws Exception {
		if (args.length != 1) {
			System.err.println("args are <customerName>");
			System.exit(1);
		}

		UtilImpl.APPS_JAR_DIRECTORY = "/Users/mike/_/skyve/skyve-war/deployments/skyve.war/WEB-INF/classes/";
		AbstractRepository.set(new LocalDesignRepository());
		Customer customer = AbstractRepository.get().getCustomer(customerName);

System.out.println(visitModules(args[0]));
	}
*/
	public static @Nonnull List<Automation> visitMenus(@Nonnull User user,
														@Nonnull String uxui,
														@Nonnull UserAgentType userAgentType,
														@Nullable TestStrategy testStrategy) {
		return visitMenus(user, null, null, uxui, userAgentType, testStrategy);
	}

	public static @Nonnull List<Automation> visitMenus(@Nonnull User user,
														@Nullable String loginPassword,
														@Nonnull String uxui,
														@Nonnull UserAgentType userAgentType,
														@Nullable TestStrategy testStrategy) {
		return visitMenus(user, null, loginPassword, uxui, userAgentType, testStrategy);
	}

	public static @Nonnull List<Automation> visitMenus(@Nonnull User user,
														@Nullable String loginCustomer,
														@Nullable String loginPassword,
														@Nonnull String uxui,
														@Nonnull UserAgentType userAgentType,
														@Nullable TestStrategy testStrategy) {
		ProvidedRepositoryFactory.getNullSafe().resetMenus(user);
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
			Menu menu = moduleMenu(u, moduleName);
			menu(u, c, m, null, menu.getItems(), uxui, interactions);

			// some modules may have no access at all for this user
			if (! interactions.isEmpty()) {
				if (loginPassword != null) {
					addLoginAndOut(automation, user, loginCustomer, loginPassword);
				}
				result.add(automation);
			}
		}

		return result;
	}

	public static @Nonnull Automation visitMenu(@Nonnull User user,
												@Nonnull String moduleName,
												@Nonnull String uxui,
												@Nonnull UserAgentType userAgentType,
												@Nullable TestStrategy testStrategy) {
		return visitMenu(user, null, null, moduleName, uxui, userAgentType, testStrategy);
	}	

	public static @Nonnull Automation visitMenu(@Nonnull User user,
												@Nullable String loginPassword,
												@Nonnull String moduleName,
												@Nonnull String uxui,
												@Nonnull UserAgentType userAgentType,
												@Nullable TestStrategy testStrategy) {
		return visitMenu(user, null, loginPassword, moduleName, uxui, userAgentType, testStrategy);
	}
	
	public static @Nonnull Automation visitMenu(@Nonnull User user,
												@Nullable String loginCustomer,
												@Nullable String loginPassword,
												@Nonnull String moduleName,
												@Nonnull String uxui,
												@Nonnull UserAgentType userAgentType,
												@Nullable TestStrategy testStrategy) {
		ProvidedRepositoryFactory.getNullSafe().resetMenus(user);
		UserImpl u = (UserImpl) user;
		Customer c = user.getCustomer();
		
		Module m = c.getModule(moduleName);
		Automation result = new Automation();
		result.setUserAgentType(userAgentType);
		result.setUxui(uxui);
		result.setTestStrategy(testStrategy);
		List<Interaction> interactions = result.getInteractions();
		Menu menu = moduleMenu(u, moduleName);
		menu(u, c, m, null, menu.getItems(), uxui, interactions);

		if (loginPassword != null) {
			addLoginAndOut(result, user, loginCustomer, loginPassword);
		}

		return result;
	}

	@SuppressWarnings("java:S3776") // Complexity OK
	private static void menu(@Nonnull User u,
								@Nonnull Customer c,
								@Nonnull Module m,
								@Nullable String descriptionPrefix,
								@Nonnull List<MenuItem> items,
								@Nonnull String uxui,
								@Nonnull List<Interaction> interactions) {
		String moduleName = m.getName();
		for (MenuItem item : items) {
			if (item.isApplicable(uxui)) {
				String description = item.getLocalisedName();
				if (descriptionPrefix != null) {
					description = String.format("%s::%s", descriptionPrefix, description);
				}

				if (item instanceof MenuGroup menuGroup) {
					menu(u, c, m, description, (menuGroup).getItems(), uxui, interactions);
				}
				else {
					Interaction interaction = new Interaction();
					interaction.setName("Menu " + description);
					List<Step> steps = interaction.getSteps();
					
					if (item instanceof ListItem list) {
						String queryName = list.getQueryName();
						String documentName = list.getDocumentName();
						String modelName = list.getModelName();

						NavigateList navigate = new NavigateList();
						navigate.setModuleName(moduleName);
						steps.add(navigate);

						Document d = null;
						
						if (queryName != null) {
							MetaDataQueryDefinition query = m.getMetaDataQuery(queryName);
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
							if (documentName == null) { // validated when the menu is loaded
								throw new MetaDataException("List menu item " + item.getName() + " in module " + moduleName + " has no query or document");
							}
							navigate.setDocumentName(documentName);
							d = m.getDocument(c, documentName);
							if (modelName != null) {
								navigate.setModelName(modelName);
								ListModel<?> model = d.getListModel(c, modelName, false);
								Document drivingDocument = model.getDrivingDocument();
								if (drivingDocument == null) {
									throw new MetaDataException("List model " + modelName + " in document " + documentName + " has no driving document");
								}
								d = drivingDocument;
							}
						}
						
						crud(u, c, m, d, uxui, navigate, steps);
					}
					else if (item instanceof EditItem edit) {
						NavigateEdit navigate = new NavigateEdit();
						navigate.setModuleName(moduleName);
						String documentName = edit.getDocumentName();
						if (documentName == null) { // validated when the menu is loaded
							throw new MetaDataException("Edit menu item " + item.getName() + " in module " + moduleName + " has no document");
						}
						navigate.setDocumentName(documentName);
						steps.add(navigate);

						Document d = m.getDocument(c, documentName);
						edit(u, c, m, d, uxui, steps);
					}

					interactions.add(interaction);
				}
			}
		}
	}
	
	public static @Nonnull List<Automation> visitModules(@Nonnull User user,
															@Nonnull String uxui,
															@Nonnull UserAgentType userAgentType,
															@Nullable TestStrategy testStrategy)
	throws Exception {
		return visitModules(user, null, null, uxui, userAgentType, testStrategy);
	}
	
	public static @Nonnull List<Automation> visitModules(@Nonnull User user,
															@Nullable String loginPassword,
															@Nonnull String uxui,
															@Nonnull UserAgentType userAgentType,
															@Nullable TestStrategy testStrategy)
	throws Exception {
		return visitModules(user, null, loginPassword, uxui, userAgentType, testStrategy);
	}
	
	public static @Nonnull List<Automation> visitModules(@Nonnull User user,
															@Nullable String loginCustomer,
															@Nullable String loginPassword,
															@Nonnull String uxui,
															@Nonnull UserAgentType userAgentType,
															@Nullable TestStrategy testStrategy)
	throws Exception {
		List<Automation> result = new ArrayList<>();
		
		Customer c = user.getCustomer();
		for (Module m : c.getModules()) {
			String moduleName = m.getName();
			// Don't process the test module as its cyclic and not required anyway
			if ("test".equals(moduleName)) {
				continue;
			}
			Automation automation = visitModule(user, 
													loginCustomer,
													loginPassword,
													moduleName,
													uxui,
													userAgentType,
													testStrategy);
			if (! automation.getInteractions().isEmpty()) {
				result.add(automation);
			}
		}
		
		return result;
	}

	public static @Nonnull Automation visitModule(@Nonnull User user,
													@Nonnull String moduleName,
													@Nonnull String uxui,
													@Nonnull UserAgentType userAgentType,
													@Nullable TestStrategy testStrategy)
	throws Exception {
		return visitModule(user, null, null, moduleName, uxui, userAgentType, testStrategy);
	}
	
	public static @Nonnull Automation visitModule(@Nonnull User user,
													@Nullable String loginPassword,
													@Nonnull String moduleName,
													@Nonnull String uxui,
													@Nonnull UserAgentType userAgentType,
													@Nullable TestStrategy testStrategy)
	throws Exception {
		return visitModule(user, null, loginPassword, moduleName, uxui, userAgentType, testStrategy);
	}
	
	public static @Nonnull Automation visitModule(@Nonnull User user,
													@Nullable String loginCustomer,
													@Nullable String loginPassword,
													@Nonnull String moduleName,
													@Nonnull String uxui,
													@Nonnull UserAgentType userAgentType,
													@Nullable TestStrategy testStrategy) {
		Customer c = user.getCustomer();
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
						user.canAccessDocument(d)) {
					if (d.getPersistent() == null) {
						Interaction interaction = new Interaction();
						interaction.setName("Edit document " + documentName);
						List<Step> steps = interaction.getSteps();
						
						NavigateEdit navigate = new NavigateEdit();
						navigate.setModuleName(moduleName);
						navigate.setDocumentName(documentName);
						steps.add(navigate);

						edit(user, c, m, d, uxui, steps);
						
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
						
						crud(user, c, m, d, uxui, navigate, steps);

						interactions.add(interaction);
					}
				}
			}
		}
		
		if (loginPassword != null) {
			addLoginAndOut(result, user, loginCustomer, loginPassword);
		}

		return result;
	}

	private static void edit(@Nonnull User u, @Nonnull Customer c, @Nonnull Module m, @Nonnull Document d, @Nonnull String uxui, @Nonnull List<Step> steps) {
		GenerateViewVisitor visitor = new GenerateViewVisitor(u, c, m, d, uxui);
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
	
	private static void crud(@Nonnull User u, @Nonnull Customer c, @Nonnull Module m, @Nonnull Document d, @Nonnull String uxui, @Nonnull NavigateList list, @Nonnull List<Step> steps) {
//System.out.println(String.format("CRUD %s.%s", m.getName(), d.getName()));
		if (u.canCreateDocument(d)) {
			ListGridNew nu = new ListGridNew();
			nu.setModuleName(list.getModuleName());
			nu.setDocumentName(list.getDocumentName());
			nu.setQueryName(list.getQueryName());
			nu.setModelName(list.getModelName());
			steps.add(nu);
			
			GenerateViewVisitor visitor = new GenerateViewVisitor(u, c, m, d, uxui);
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
			else {
				steps.addAll(visitor.getDeleteComments());
			}
		}
	}
	
	/**
	 * Returns the user's menu for a module, which resetting the menus put in place for every module of the customer.
	 *
	 * @param u the user whose menus were reset
	 * @param moduleName the name of a module of the user's customer
	 * @return the module menu
	 */
	private static @Nonnull Menu moduleMenu(@Nonnull UserImpl u, @Nonnull String moduleName) {
		Menu result = u.getModuleMenu(moduleName);
		if (result == null) {
			throw new IllegalStateException("User " + u.getName() + " has no menu for module " + moduleName);
		}
		return result;
	}

	private static void addLoginAndOut(@Nonnull Automation automation, @Nonnull User user, @Nullable String customer, @Nonnull String password) {
		Procedure before = automation.getBefore();
		String userName = user.getName();
		if (userName == null) {
			throw new IllegalStateException("Cannot log in to the automation as the user has no name");
		}
		Login login = new Login();
		login.setCustomer(customer);
		login.setUser(userName);
		login.setPassword(password);
		if (before == null) {
			before = new Procedure();
			automation.setBefore(before);
		}
		List<Step> beforeSteps = before.getSteps();
		beforeSteps.add(0, login);
		// The router may not route the browser to SmartClient, so switch to it explicitly
		if (UxUi.DESKTOP_NAME.equals(automation.getUxui())) {
			beforeSteps.add(1, new SwitchMode());
		}

		Procedure after = automation.getAfter();
		if (after == null) {
			after = new Procedure();
			automation.setAfter(after);
		}
		after.getSteps().add(new Logout());
	}
}
