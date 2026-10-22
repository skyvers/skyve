package org.skyve.impl.generate.sail;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.NoOpViewVisitor;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.interaction.TabSelect;
import org.skyve.metadata.sail.language.step.interaction.TestDataEnter;
import org.skyve.metadata.sail.language.step.interaction.actions.Action;
import org.skyve.metadata.sail.language.step.interaction.actions.ZoomOut;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridNew;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Binder.TargetMetaData;

import jakarta.annotation.Nonnull;

/**
 * View visitor that traverses a Skyve view model and emits a SAIL
 * (Skyve Automated Integration Language) test script.
 *
 * <p>Used by {@link Generator} to produce SAIL scripts from existing view metadata.
 */
public class GenerateViewVisitor extends NoOpViewVisitor {
	private List<Step> populateSteps = new ArrayList<>();
	private List<Step> actionSteps = new ArrayList<>();
	private boolean hasSave = false;
	private boolean hasOk = false;
	private boolean hasCancel = false;
	private boolean hasDelete = false;
	// list of module.document.view.binding visit in data grids to stop infinite recursion
	private Set<String> breadcrumbs;
	
	protected GenerateViewVisitor(@Nonnull Customer customer, 
										@Nonnull Module module,
										@Nonnull Document document,
										@Nonnull String uxui) {
		super((CustomerImpl) customer, 
				(ModuleImpl) module,
				(DocumentImpl) document,
				determineView(customer, document, uxui),
				uxui);
		breadcrumbs = new TreeSet<>();
		populateSteps.add(new TestDataEnter());
	}
	
	private GenerateViewVisitor(@Nonnull Customer customer, 
									@Nonnull Module module,
									@Nonnull Document document,
									@Nonnull String uxui,
									@Nonnull Set<String> breadcrumbs) {
		super((CustomerImpl) customer, 
				(ModuleImpl) module,
				(DocumentImpl) document,
				determineView(customer, document, uxui),
				uxui);
		this.breadcrumbs = breadcrumbs;
		populateSteps.add(new TestDataEnter());
	}

	private static @Nonnull ViewImpl determineView(@Nonnull Customer c, @Nonnull Document d, @Nonnull String uxui) {
		ViewImpl createView = (ViewImpl) d.getView(uxui, c, ViewType.create.toString());
		ViewImpl editView = (ViewImpl) d.getView(uxui, c, ViewType.edit.toString());
		
		return (createView != editView) ? createView : editView;
	}

	@Override
	public void visitTab(@Nonnull Tab tab, boolean parentVisible, boolean parentEnabled) {
		// A tab is selected by its title, which is null when the view metadata gives it a blank title
		String title = tab.getLocalisedTitle();
		if (title == null) {
			throw new MetaDataException("A tab in view " + view.getName() + " of document " + document.getName() + " has no title to select it by");
		}
		TabSelect select = new TabSelect();
		select.setTabPath(title);
		populateSteps.add(select);
		actionSteps.add(select);
	}
	
	@Override
	public void visitDataGrid(@Nonnull DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		// Do nothing if we can't add a row
		if (Boolean.FALSE.equals(grid.getEditable()) || Boolean.FALSE.equals(grid.getShowAdd())) {
			return;
		}

		String binding = grid.getBinding();
		if (binding == null) { // validated when the view is loaded
			throw new MetaDataException("A data grid in view " + view.getName() + " of document " + document.getName() + " has no binding");
		}

		// Do nothing if we've visited this grid on this view before
		String breadcrumb = String.format("%s.%s.%s.%s", module.getName(), document.getName(), view.getName(), binding);
		if (! breadcrumbs.add(breadcrumb)) {
			return;
		}
		
		DataGridNew nu = new DataGridNew();
		nu.setBinding(binding);
		populateSteps.add(nu);
		
		TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
		Relation relation = (Relation) target.getAttribute();
		if (relation != null) { // should always be
			Document gridDocument = module.getDocument(customer, relation.getDocumentName());
			Module gridModule = customer.getModule(gridDocument.getOwningModuleName());
			GenerateViewVisitor gridVisitor = new GenerateViewVisitor(customer, gridModule, gridDocument, currentUxUi, breadcrumbs);
			gridVisitor.visit();
			populateSteps.addAll(gridVisitor.populateSteps);

			populateSteps.add(new ZoomOut());
		}
	}
	
	@Override
	public void visitSaveAction(@Nonnull ActionImpl action) {
		hasSave = true;
	}
	
	@Override
	public void visitOKAction(@Nonnull ActionImpl action) {
		hasOk = true;
	}

	@Override
	public void visitCancelAction(@Nonnull ActionImpl action) {
		hasCancel = true;
	}

	@Override
	public void visitDeleteAction(@Nonnull ActionImpl action) {
		hasDelete = true;
	}
	
	@Override
	public void visitCustomAction(@Nonnull ActionImpl action) {
		if (! Boolean.FALSE.equals(action.getInActionPanel())) {
			addCustomAction(action);
		}
	}
	
	@Override
	public void visitButton(@Nonnull Button button, boolean parentVisible, boolean parentEnabled) {
		String actionName = button.getActionName();
		ActionImpl action = (ActionImpl) view.getAction(actionName);
		if (action == null) { // validated when the view is loaded
			throw new MetaDataException("Button action " + actionName + " does not exist in view " + view.getName() + " of document " + document.getName());
		}
		addCustomAction(action);
	}
	
	private void addCustomAction(@Nonnull ActionImpl action) {
		Action step = new Action();
		step.setActionName(action.getName());
		if (action.getConfirmationText() != null) {
			step.setConfirm(Boolean.TRUE);
		}
		actionSteps.add(step);
	}
	
	@Nonnull List<Step> getPopulateSteps() {
		return populateSteps;
	}
	
	@Nonnull List<Step> getActionSteps() {
		return actionSteps;
	}
	
	boolean getHasSave() {
		return hasSave && document.isPersistable();
	}
	
	boolean getHasOk() {
		return hasOk && document.isPersistable();
	}
	
	boolean getHasCancel() {
		return hasCancel;
	}
	
	boolean getHasDelete() {
		return hasDelete && document.isPersistable();
	}
}
