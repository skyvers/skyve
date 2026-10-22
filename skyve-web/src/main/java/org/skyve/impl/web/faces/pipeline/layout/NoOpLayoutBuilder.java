package org.skyve.impl.web.faces.pipeline.layout;

import java.util.List;

import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.faces.component.UIComponent;

public class NoOpLayoutBuilder extends LayoutBuilder {
	@Override
	public @Nullable UIComponent viewLayout(@Nullable UIComponent component) {
		return component;
	}

	@Override
	public @Nullable List<UIComponent> toolbarLayouts(@Nullable List<UIComponent> components) {
		return components;
	}

	@Override
	public @Nullable UIComponent tabLayout(@Nullable UIComponent component) {
		return component;
	}

	@Override
	public @Nullable UIComponent vboxLayout(@Nullable UIComponent component, @Nonnull VBox vbox) {
		return component;
	}

	@Override
	public @Nullable UIComponent hboxLayout(@Nullable UIComponent component, @Nonnull HBox hbox) {
		return component;
	}
	
	@Override
	public @Nullable UIComponent sidebarLayout(@Nullable UIComponent component, @Nonnull Sidebar sidebar, boolean createView) {
		return component;
	}

	@Override
	public @Nullable UIComponent formLayout(@Nullable UIComponent component, @Nonnull Form form) {
		return component;
	}

	@Override
	public @Nullable UIComponent formRowLayout(@Nullable UIComponent component, @Nonnull FormRow row) {
		return component;
	}

	@Override
	public void addToolbarLayouts(@Nonnull List<UIComponent> toolbars, @Nonnull List<UIComponent> toolbarLayouts) {
		// do nothing
	}

	@Override
	public void addToolbarsOrLayouts(@Nonnull UIComponent view, @Nonnull List<UIComponent> toolbarsOrLayouts) {
		// do nothing
	}

	@Override
	public @Nullable UIComponent addTabLayout(@Nullable UIComponent component, @Nonnull UIComponent tab, @Nonnull UIComponent tabLayout) {
		return component;
	}

	@Override
	public void addTab(@Nonnull UIComponent tabPane, @Nonnull UIComponent tab) {
		// do nothing
	}

	@Override
	public @Nullable UIComponent addedTab(@Nullable UIComponent component, @Nonnull UIComponent tab) {
		return component;
	}

	@Override
	public void addBorderLayout(@Nonnull UIComponent border, @Nonnull UIComponent borderLayout) {
		// do nothing
	}

	@Override
	public @Nullable UIComponent addedBorderLayout(@Nullable UIComponent component, @Nonnull UIComponent borderLayout) {
		return component;
	}

	@Override
	public @Nullable UIComponent addFormRowLayout(@Nullable UIComponent component, @Nonnull UIComponent formLayout, @Nonnull UIComponent rowLayout) {
		return component;
	}

	@Override
	public @Nullable UIComponent addedFormRowLayout(@Nullable UIComponent component, @Nonnull UIComponent rowLayout) {
		return component;
	}

	/**
	 * Ignores form-item label layout requests.
	 *
	 * @param formOrRowLayout target form or row layout
	 * @param formItemComponent form-item component
	 * @param currentForm current form metadata
	 * @param currentFormItem current form-item metadata
	 * @param currentFormColumn current form-column metadata
	 * @param widgetLabel raw label text
	 * @param widgetEscapeLabel resolved label escape decision
	 * @param widgetRequiredMessage required-message text normalised for unescaped
	 *        PrimeFaces message rendering
	 * @param widgetEscapeRequiredMessage resolved escape decision retained for layout
	 * @param widgetInvisible invisible-condition expression
	 * @param widgetHelpText help text
	 */
	@Override
	public void layoutFormItemLabel(@Nonnull UIComponent formOrRowLayout,
										@Nonnull UIComponent formItemComponent,
										@Nonnull Form currentForm,
										@Nonnull FormItem currentFormItem,
										@Nonnull FormColumn currentFormColumn,
										@Nullable String widgetLabel,
										boolean widgetEscapeLabel,
										@Nullable String widgetRequiredMessage,
										boolean widgetEscapeRequiredMessage,
										@Nullable String widgetInvisible,
										@Nullable String widgetHelpText) {
		// do nothing
	}

	/**
	 * Ignores form-item widget layout requests.
	 *
	 * @param formOrRowLayout target form or row layout
	 * @param formItemComponent form-item component
	 * @param currentForm current form metadata
	 * @param currentFormItem current form-item metadata
	 * @param currentFormColumn current form-column metadata
	 * @param widgetLabel raw label text
	 * @param widgetEscapeLabel resolved label escape decision
	 * @param widgetColspan widget column span
	 * @param widgetRequiredMessage required-message text normalised for unescaped
	 *        PrimeFaces message rendering
	 * @param widgetEscapeRequiredMessage resolved escape decision retained for layout
	 * @param widgetInvisible invisible-condition expression
	 * @param widgetHelpText help text
	 * @param widgetEscapeHelp resolved help escape decision
	 * @param widgetPixelWidth optional pixel width
	 * @param showLabel whether labels are shown
	 * @param topLabel whether labels render above widgets
	 */
	@Override
	public void layoutFormItemWidget(@Nonnull UIComponent formOrRowLayout,
										@Nonnull UIComponent formItemComponent,
										@Nonnull Form currentForm,
										@Nonnull FormItem currentFormItem,
										@Nonnull FormColumn currentFormColumn,
										@Nullable String widgetLabel,
										boolean widgetEscapeLabel,
										int widgetColspan,
										@Nullable String widgetRequiredMessage,
										boolean widgetEscapeRequiredMessage,
										@Nullable String widgetInvisible,
										@Nullable String widgetHelpText,
										boolean widgetEscapeHelp,
										@Nullable Integer widgetPixelWidth,
										boolean showLabel,
										boolean topLabel) {
		// do nothing
	}

	@Override
	public @Nullable UIComponent contentSignatureLayout(@Nullable UIComponent component, @Nonnull ContentSignature signature) {
		return component;
	}
	
	@Override
	public @Nullable UIComponent addToContainer(@Nullable UIComponent component, 
													@Nonnull Container viewContainer,
													@Nonnull UIComponent container,
													@Nullable UIComponent componentToAdd,
													@Nullable Integer pixelWidth,
													@Nullable Integer responsiveWidth,
													@Nullable Integer percentageWidth,
													@Nullable Integer sm,
													@Nullable Integer md,
													@Nullable Integer lg,
													@Nullable Integer xl,
													@Nullable String invisibleCondition) {
		return component;
	}

	@Override
	public @Nullable UIComponent addedToContainer(@Nullable UIComponent component, @Nonnull Container viewContainer, @Nonnull UIComponent container) {
		return component;
	}
}
