package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.component.Component;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.impl.metadata.view.widget.bound.input.Comparison;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.metadata.MetaData;

abstract class FluentContainer<T extends FluentContainer<T>> extends FluentWidget {
	protected FluentContainer() {
		// nothing to see
	}
	
	@SuppressWarnings("unchecked")
	protected T from(Container container) {
		for (MetaData widget : container.getContained()) {
			if (widget instanceof StaticImage) {
				addStaticImage(new FluentStaticImage().from((StaticImage) widget));
			}
			else if (widget instanceof VBox) {
				addVBox(new FluentVBox().from((VBox) widget));
			}
			else if (widget instanceof HBox) {
				addHBox(new FluentHBox().from((HBox) widget));
			}
			else if (widget instanceof Form) {
				addForm(new FluentForm().from((Form) widget));
			}
			else if (widget instanceof TabPane) {
				addTabPane(new FluentTabPane().from((TabPane) widget));
			}
			else if (widget instanceof Label) {
				addLabel(new FluentLabel().from((Label) widget));
			}
			else if (widget instanceof Blurb) {
				addBlurb(new FluentBlurb().from((Blurb) widget));
			}
			else if (widget instanceof Button) {
				addButton(new FluentButton().from((Button) widget));
			}
			else if (widget instanceof DataGrid) {
				addDataGrid(new FluentDataGrid().from((DataGrid) widget));
			}
			else if (widget instanceof ListGrid) {
				addListGrid(new FluentListGrid().from((ListGrid) widget));
			}
			else if (widget instanceof Component) {
				addComponent(new FluentComponent().from((Component) widget));
			}
			else if (widget instanceof ZoomIn) {
				addZoomIn(new FluentZoomIn().from((ZoomIn) widget));
			}
			else if (widget instanceof Chart) {
				addChart(new FluentChart().from((Chart) widget));
			}
			else if (widget instanceof MapDisplay) {
				addMapDisplay(new FluentMapDisplay().from((MapDisplay) widget));
			}
			else if (widget instanceof DynamicImage) {
				addDynamicImage(new FluentDynamicImage().from((DynamicImage) widget));
			}
			else if (widget instanceof DialogButton) {
				addDialogButton(new FluentDialogButton().from((DialogButton) widget));
			}
			else if (widget instanceof Link) {
				addLink(new FluentLink().from((Link) widget));
			}
			else if (widget instanceof Spacer) {
				addSpacer(new FluentSpacer().from((Spacer) widget));
			}
			else if (widget instanceof ListMembership) {
				addListMembership(new FluentListMembership().from((ListMembership) widget));
			}
			else if (widget instanceof CheckMembership) {
				addCheckMembership(new FluentCheckMembership().from((CheckMembership) widget));
			}
			else if (widget instanceof Comparison) {
				addComparison(new FluentComparison().from((Comparison) widget));
			}
			else if (widget instanceof TreeGrid) {
				addTreeGrid(new FluentTreeGrid().from((TreeGrid) widget));
			}
			else if (widget instanceof DataRepeater) {
				addDataRepeater(new FluentDataRepeater().from((DataRepeater) widget));
			}
			else if (widget instanceof ListRepeater) {
				addListRepeater(new FluentListRepeater().from((ListRepeater) widget));
			}
			else if (widget instanceof Inject) {
				addInject(new FluentInject().from((Inject) widget));
			}
			else {
				throw new IllegalStateException(widget + " is not catered for");
			}
		}
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T addStaticImage(FluentStaticImage image) {
		get().getContained().add(image.get());
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T addVBox(FluentVBox vbox) {
		get().getContained().add(vbox.get());
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T addHBox(FluentHBox hbox) {
		get().getContained().add(hbox.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addForm(FluentForm form) {
		get().getContained().add(form.get());
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T addTabPane(FluentTabPane tabPane) {
		get().getContained().add(tabPane.get());
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T addButton(FluentButton button) {
		get().getContained().add(button.get());
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T addZoomIn(FluentZoomIn zoomIn) {
		get().getContained().add(zoomIn.get());
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T addChart(FluentChart chart) {
		get().getContained().add(chart.get());
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T addMapDisplay(FluentMapDisplay map) {
		get().getContained().add(map.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addDynamicImage(FluentDynamicImage image) {
		get().getContained().add(image.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addDialogButton(FluentDialogButton button) {
		get().getContained().add(button.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addLabel(FluentLabel label) {
		get().getContained().add(label.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addBlurb(FluentBlurb blurb) {
		get().getContained().add(blurb.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addLink(FluentLink link) {
		get().getContained().add(link.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addSpacer(FluentSpacer spacer) {
		get().getContained().add(spacer.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addListMembership(FluentListMembership list) {
		get().getContained().add(list.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addCheckMembership(FluentCheckMembership check) {
		get().getContained().add(check.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addComparison(FluentComparison comparison) {
		get().getContained().add(comparison.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addDataGrid(FluentDataGrid grid) {
		get().getContained().add(grid.get());
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T addListGrid(FluentListGrid grid) {
		get().getContained().add(grid.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addTreeGrid(FluentTreeGrid grid) {
		get().getContained().add(grid.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addDataRepeater(FluentDataRepeater repeater) {
		get().getContained().add(repeater.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addListRepeater(FluentListRepeater repeater) {
		get().getContained().add(repeater.get());
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T addInject(FluentInject inject) {
		get().getContained().add(inject.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addComponent(FluentComponent component) {
		get().getContained().add(component.get());
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T removeContained(int index) {
		get().getContained().remove(index);
		return (T) this;
	}
	
	@Override
	public abstract Container get();
}
