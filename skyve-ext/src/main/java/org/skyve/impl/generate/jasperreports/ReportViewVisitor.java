package org.skyve.impl.generate.jasperreports;

import java.util.ArrayList;
import java.util.List;

import org.skyve.domain.types.Decimal5;
import org.skyve.impl.generate.jasperreports.Container.ContainerType;
import org.skyve.impl.generate.jasperreports.DesignSpecification.DefinitionSource;
import org.skyve.impl.generate.jasperreports.DesignSpecification.ReportType;
import org.skyve.impl.generate.jasperreports.ReportElement.ElementAlignment;
import org.skyve.impl.generate.jasperreports.ReportElement.ElementType;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.ViewVisitor;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.event.Addable;
import org.skyve.impl.metadata.view.event.Changeable;
import org.skyve.impl.metadata.view.event.Editable;
import org.skyve.impl.metadata.view.event.EventSource;
import org.skyve.impl.metadata.view.event.Focusable;
import org.skyve.impl.metadata.view.event.Removable;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.Selectable;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;
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
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.Comparison;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.ContentUpload;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.AbstractDataWidget;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * View visitor that extracts field and parameter descriptors from a Skyve view
 * model for use in JasperReports design generation.
 */
public class ReportViewVisitor extends ViewVisitor {
	public static final double TWIP_TO_PIXEL = 0.0666666667;
	public static final double PIXEL_TO_TWIP = 15;

	protected ReportDesignGenerator reportDesignGenerator;
	protected DesignSpecification design = null;
	protected List<ReportBand> detailBands = null;
	
	protected DesignSpecification sub  = null;
	protected List<ReportBand> subreportBands = null;
	protected ReportBand band = null;
	protected int containerDepth = 0;
	protected int col = 0;
	protected int row = 0;
	protected Boolean showLabel = null;
	protected Boolean forceValueBold = null;
	protected Boolean forceValueItalic = null;
	protected ReportElement.ElementAlignment itemAlignment = null;
	protected ReportElement.ElementAlignment labelAlignment = null;
	protected boolean subreport = false;
	protected boolean inForm = false;
	protected String viewTitle = null;
	private boolean visited = false;

	protected int top = 0;
	protected int left = 0;
	protected int width = 0;

	protected Container currentContainer = null;

	/**
	 * Indicates whether the source view has already been traversed.
	 *
	 * @return {@code true} after {@link #visit()} has been executed.
	 */
	public boolean isVisited() {
		return visited;
	}

	/**
	 * Marks whether the visitor pass has completed.
	 *
	 * @param visited The visited flag value.
	 */
	public void setVisited(boolean visited) {
		this.visited = visited;
	}

	/**
	 * Returns the localised title of the visited view.
	 *
	 * @return The view title used for report headings.
	 */
	public @Nullable String getViewTitle() {
		return viewTitle;
	}

	/**
	 * Returns whether implicit labels are rendered for value widgets.
	 *
	 * @return {@code true} when label elements should be emitted.
	 */
	public @Nullable Boolean getShowLabel() {
		return showLabel;
	}

	/**
	 * Controls whether value widgets should emit implicit label elements.
	 *
	 * @param showLabel The label rendering flag.
	 */
	public void setShowLabel(@Nullable Boolean showLabel) {
		this.showLabel = showLabel;
	}

	/**
	 * Sets the mutable design being populated during view traversal.
	 *
	 * @param reportDesign The design context for this visitor run.
	 */
	public void setDesign(@Nonnull DesignSpecification reportDesign) {
		design = reportDesign;
	}


	/**
	 * Returns the detail bands produced by the most recent visit pass.
	 *
	 * @return Mutable list of generated detail bands.
	 */
	public @Nonnull List<ReportBand> getDetailBands() {
		return detailBands;
	}

	/**
	 * Calculates a TWIP size from pixel, percentage, or responsive width inputs.
	 *
	 * <p>Priority is applied in order: pixel, then percentage, then responsive.
	 *
	 * @param containerSizeInTwips The container size used for percentage/responsive calculations.
	 * @param pixels The fixed pixel width, if supplied.
	 * @param percentage The percentage width, if supplied.
	 * @param responsive The 12-column responsive width, if supplied.
	 * @return The computed size in TWIPs, or {@code null} when no sizing input is supplied.
	 */
	@SuppressWarnings("boxing")
	private @Nullable Integer calculateTwipSize(@Nonnull Integer containerSizeInTwips, @Nullable Integer pixels, @Nullable Integer percentage, @Nullable Integer responsive) {
		Integer resultSizeInTwips = null; // auto set
		if (pixels != null) {
			final Decimal5 pixelToTwip = design.getPixelToTwip();
			if (pixelToTwip != null) {
				resultSizeInTwips = Double.valueOf(pixels.doubleValue() * pixelToTwip.doubleValue()).intValue();
			} else {
				resultSizeInTwips = Double.valueOf(pixels.doubleValue() * PIXEL_TO_TWIP).intValue();
			}
		} else if (percentage != null) {
			resultSizeInTwips = (containerSizeInTwips * percentage) / 100;
		} else if (responsive != null) {
			resultSizeInTwips = (containerSizeInTwips * responsive) / 12;
		}
		return resultSizeInTwips;
	}

	/**
	 * Start a report band given a widgetId and a title
	 * 
	 * @param widgetId
	 * @param title
	 */
	protected void startDetailBand(@Nullable String widgetId, @Nullable String title, @Nullable String invisibleConditionName) {

		try {
			band = new ReportBand();
			band.setBandType(ReportBand.BandType.detail);
			band.setParent(design);
			if (widgetId != null) {
				band.setName(widgetId);
			} else {
				band.setName("Detail" + detailBands.size());
			}
			band.setSplitType(design.getBandSplitType());
			band.setInvisibleConditionName(invisibleConditionName);

			// initialise current layout situation
			top = 0;
			left = 0;
			width = design.getColumnWidth().intValue();

		} catch (Exception e) {
			LOGGER.warn("COULD NOT CONSTRUCT BAND {} FOR WIDGET_ID {}", String.valueOf(detailBands.size()), widgetId, e);
		}
	}

	/**
	 * Finish the band and perform the automated layout
	 */
	protected void finishDetailBand() {

		if (currentContainer != null) {

			// layout and add to band
			layout(currentContainer);
			band.addContainer(currentContainer);
		}

		if (!band.getElements().isEmpty()) {
			detailBands.add(band);
		}
		band = null;
	}

	/**
	 * Add a layout container, and set it (descend in) as the current container
	 * 
	 * @param widgetId
	 * @param borderTitle
	 * @param border
	 * @param pixelWidth
	 * @param percentageWidth
	 * @param responsiveWidth
	 * @param horizontal
	 * @param name
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	protected void addContainer(@Nullable String widgetId, @Nullable String borderTitle, @Nullable Boolean border, @Nullable Integer pixelWidth, @Nullable Integer percentageWidth, @Nullable Integer responsiveWidth, @Nonnull Boolean horizontal,
			@Nonnull ContainerType type, @Nullable String invisibleConditionName) {

		if (currentContainer != null
				&& (Boolean.TRUE.equals(design.getVerticalise()))
				&& !inForm) {
			handleEndContainer();
		}

		if (currentContainer == null) {
			currentContainer = new Container(Integer.valueOf(top), Integer.valueOf(left), null, horizontal.booleanValue(), type);
			currentContainer.setPixelWidth(pixelWidth);
			currentContainer.setPercentageWidth(percentageWidth);
			currentContainer.setResponsiveWidth(responsiveWidth);
			currentContainer.setBorder(border);
			currentContainer.setBorderTitle(borderTitle);

			if (containerDepth == 0 || Boolean.TRUE.equals(design.getVerticalise()) && !inForm) {
				startDetailBand(widgetId, borderTitle, invisibleConditionName);
			}

		} else {
			Container container = new Container(Integer.valueOf(top), Integer.valueOf(left), null, horizontal.booleanValue(), type);
			container.setPixelWidth(pixelWidth);
			container.setPercentageWidth(percentageWidth);
			container.setResponsiveWidth(responsiveWidth);
			container.setBorder(border);
			container.setBorderTitle(borderTitle);

			currentContainer.addContainer(container);
			currentContainer = container; // recurse -> descend into new child
		}
		containerDepth++;

	}

	/**
	 * Handle what needs to happen after a container has been visited,
	 * if the container was at depth 0 in the structure, end the band
	 * Otherwise, recurse back up to it's parent
	 */
	protected void handleEndContainer() {

		if (currentContainer != null) {

			// if end of band or "Verticalise", finish current band
			if (currentContainer.getDepth() == 0
					|| (Boolean.TRUE.equals(design.getVerticalise()) && !inForm)) {

				finishDetailBand();
				currentContainer = null;
				containerDepth = 0;

			} else {

				// recurse back up
				containerDepth--;
				if (currentContainer != null) {
					currentContainer = currentContainer.parent;
				}
			}
		}
	}

	/**
	 * Calculate positions of all report elements and perform autosizing and stretch on elements which are unsized
	 * 
	 * @param container
	 */
	@SuppressWarnings({"boxing", "java:S3776", "java:S6541"}) // complexity OK
	protected void layout(@Nonnull Container container) {
		
		int sizedCols = 0;
		int borderTitleHeight  = 0;

		// calculate a specified width if one is supplied, otherwise stretch to fill container width
		Integer parentWidth;
		final Container parentContainer = container.getParent();
		final Integer parentContainerWidth = (parentContainer == null) ? null : parentContainer.getWidth();
		if (parentContainerWidth == null) {
			parentWidth = design.getColumnWidth();
		} else {
			parentWidth = parentContainerWidth;
		}
		// in Verticalise mode - stretch horizontally
		if (!Boolean.TRUE.equals(design.getVerticalise()) || inForm) {
			Integer calcWidth = calculateTwipSize(parentWidth, container.getPixelWidth(), container.getPercentageWidth(), container.getResponsiveWidth());
			if (calcWidth != null) {
				container.setWidth(calcWidth);
			}
		}
		Integer containerWidth = container.getWidth();
		if (containerWidth == null) {
			containerWidth = parentWidth;
			container.setWidth(containerWidth);
		}
		int remainingWidth = containerWidth.intValue();

		container.setHeight(Integer.valueOf(0));
		// containers are created with a top and children are positioned before they are laid out
		final Integer containerTop = container.getTop();
		if (containerTop == null) {
			throw new IllegalStateException("Container " + container.getContainerType() + " has no top to lay out from");
		}
		container.setVerticalPosition(containerTop.intValue());

		// BORDER TITLE
		// Border Title, (border done once height is calculated)
		if (Boolean.TRUE.equals(container.getBorder())) {

			if (container.getBorderTitle() != null) {

				borderTitleHeight = design.getDefaultElementHeight();
				// containers are always created with a type
				final ContainerType containerType = container.getContainerType();
				if (containerType == null) {
					throw new IllegalStateException("Container has no type");
				}
				band = Renderer.addElement(band, ReportElement.ElementType.staticText, containerType.name() + "_title", container.getBorderTitle(), null, null,
						container.getTop(),
						container.getLeft(), container.getWidth(), borderTitleHeight, Boolean.TRUE, design.getLabelAlignmentOverride(), Boolean.TRUE, null,
						design.getSectionTitleForeground(), design.getSectionTitleBackground(), null);

				container.setVerticalPosition(container.getVerticalPosition() + borderTitleHeight);
				container.addHeight(borderTitleHeight);
			}
		}

		if (container.getContainers().isEmpty()) {

			if (ContainerType.column.equals(container.getContainerType())) {

				// LAYOUT ELEMENTS
				// Elements only occur in a column container which is always vertical
				// Note that element labels and values will occur in different column containers, so we need to compensate for empty grid
				// locations and spacers
				// row height must be the maximum of all elements in that row
				for (int currRow = 0; currRow < container.getRows(); currRow++) {

					boolean found = false;
					for (ReportElement e : container.getElements()) {

						if (rowOf(e) == currRow) {

							// this container has something inside it
							found = true;
							container.setFilled(true);

							e.setElementWidth(calculateTwipSize(containerWidth, e.getPixelWidth(), e.getPercentageWidth(), e.getResponsiveWidth()));

							// find absolute top
							e.setElementTop(container.getVerticalPosition());

							// find absolute left
							if (e.getElementLeft() == null) {
								e.setElementLeft(container.getLeft());
							}

							// stretch to fill container unless width is already set
							if (e.getElementWidth() == null) {
								e.setElementWidth(container.getWidth());
							}

							// set height if not already specified
							// TODO - calculate height if specified
							Integer elementHeight = e.getElementHeight();
							if (elementHeight == null) {
								elementHeight = design.getDefaultElementHeight();
								e.setElementHeight(elementHeight);
							}

							// update container height
							container.setVerticalPosition(container.getVerticalPosition() + elementHeight.intValue());
							container.addHeight(elementHeight);
						}
					}

					// Handle missing grid spacing - empty row within a column
					if (!found) {
						container.setVerticalPosition(container.getVerticalPosition() + design.getDefaultElementHeight());
						container.addHeight(design.getDefaultElementHeight());
					}

				}
				
			} else {

				// size non Form elements - dynamic image, static image
				int sizedElems = 0;
				// calculate remaining width when specifically sized elements are taken into account
				for (ReportElement e : container.getElements()) {
					if (e.getElementWidth() != null) {
						final Integer elementWidth = calculateTwipSize(containerWidth, e.getPixelWidth(), e.getPercentageWidth(), e.getResponsiveWidth());
						e.setElementWidth(elementWidth);
						// as for child containers, an element without a size hint is left unsized
						if (elementWidth != null) {
							remainingWidth = remainingWidth - elementWidth.intValue();
							sizedElems++;
						}
					}
				}
				int blockSize = remainingWidth;
				if (container.getElements().size() > sizedElems) {
					blockSize = remainingWidth / (container.getElements().size() - sizedElems);
				}
				for (ReportElement e : container.getElements()) {
					container.setFilled(true);

					// find absolute top
					e.setElementTop(container.getVerticalPosition());

					// find absolute left
					if (e.getElementLeft() == null) {
						e.setElementLeft(container.getLeft());
					}

					// stretch to fill container unless width is already set
					Integer elementWidth = e.getElementWidth();
					if (elementWidth == null) {
						if (container.getElements().indexOf(e) == container.getElements().size() - 1) {
							elementWidth = Integer.valueOf(remainingWidth);
						} else {
							elementWidth = Integer.valueOf(blockSize);
						}
						e.setElementWidth(elementWidth);
					}

					// set height if not already specified
					// TODO - calculate height if specified - default to square shapes
					Integer elementHeight;
					if(ElementType.subreport.equals(e.getElementType())){
						elementHeight = design.getDefaultElementHeight();
						e.setElementHeight(elementHeight);
					} 
					else {
						elementHeight = e.getElementHeight();
						if (elementHeight == null) {
							elementHeight = elementWidth;
							e.setElementHeight(elementHeight);
						}
					}

					if (Boolean.TRUE.equals(container.getHorizontal())) {
						// update container width
						final Integer containerLeft = container.getLeft();
						if (containerLeft == null) {
							throw new IllegalStateException("Container " + container.getContainerType() + " has no left to lay out from");
						}
						container.setLeft(containerLeft.intValue() + elementWidth.intValue());
						remainingWidth = remainingWidth - blockSize;
					} else {
						// update container height
						container.setVerticalPosition(container.getVerticalPosition() + elementHeight.intValue());
						container.addHeight(elementHeight);
					}
				}

			}
		} else {

			// Layout horizontal spaced items
			if (Boolean.TRUE.equals(container.getHorizontal())) {

				for (Container c : container.getContainers()) {

					// ignore unsized columns to calculate total of sized columns
					Integer colW = calculateTwipSize(containerWidth, c.getPixelWidth(), c.getPercentageWidth(), c.getResponsiveWidth());
					c.setWidth(colW);
					if (colW != null) {
						remainingWidth = remainingWidth - colW;
						sizedCols++;
					}
				}

				// now size unsized columns with remaining width
				if (sizedCols < container.getContainers().size()) {
					int autoWidth = remainingWidth / (container.getContainers().size() - sizedCols);

					int index = 0;
					for (Container c : container.getContainers()) {
						Integer colW = c.getWidth();
						if (colW == null) {
							if (index == container.getContainers().size() - 1) {
								c.setWidth(remainingWidth);
							} else {
								c.setWidth(autoWidth);
								remainingWidth = remainingWidth - autoWidth;
							}
						}
						index++;
					}

				}
			} else {
				// For vertical components, stretch to full container width
				for (Container c : container.getContainers()) {
					if (c.getWidth() == null) {
						c.setWidth(container.getWidth());
					}
				}
			}

			// and recurse to autosize children
			int index = 0;
			final Integer containerLeft = container.getLeft();
			if (containerLeft == null) {
				throw new IllegalStateException("Container " + container.getContainerType() + " has no left to lay out from");
			}
			int horizontalPosition = containerLeft.intValue();
			int verticalPosition = container.getVerticalPosition();
			int maxHeight = 0;
			for (Container c : container.getContainers()) {

				// set absolute position by calculating effect of previous containers
				if (Boolean.TRUE.equals(container.getHorizontal()) && index > 0) {
					// every child of a horizontal container has been sized above
					final Integer previousWidth = container.getContainers().get(index - 1).getWidth();
					if (previousWidth == null) {
						throw new IllegalStateException("Container " + container.getContainerType() + " has an unsized child");
					}
					horizontalPosition = horizontalPosition + previousWidth.intValue();
				} else if (index > 0) {
					if (container.getContainers().get(index - 1).getHeight() != null) {
						verticalPosition = verticalPosition + container.getContainers().get(index - 1).getVerticalPosition();
					}
				}
				c.setLeft(Integer.valueOf(horizontalPosition));
				c.setTop(verticalPosition);

				// recurse
				layout(c);

				// vertical layout
				if (Boolean.TRUE.equals(container.getHorizontal())) {

					// horizontal height = max height of all child containers
					maxHeight = Math.max(maxHeight, laidOutHeight(c));
				} else {
					// size vertical
					// Todo cleanup
					if (c.getHeight() == null) {
						c.setHeight(Integer.valueOf(0));
					}
					container.addHeight(c.getHeight());
					container.setVerticalPosition(laidOutHeight(container));
				}

				// keep track of whether children actually contain anything
				if (c.isFilled()) {
					container.setFilled(true);
				}
				index++;
			}

			// size vertical size for horizontal container is maximum height of containers
			if (Boolean.TRUE.equals(container.getHorizontal())) {
				container.addHeight(maxHeight);
				container.setVerticalPosition(laidOutHeight(container));
			}

		}
		
		//fix row heights for form
		if(ContainerType.form.equals(container.getContainerType())){
			
			//set max row height
			int[] maxRowHeight = new int[container.getRows()];
			for(int r= 0; r<container.getRows(); r++){
				maxRowHeight[r] = 0;
				for (Container child : container.getContainers()) {
					for(ReportElement e: child.getElements()){
						if(rowOf(e)==r){
							final Integer elementHeight = e.getElementHeight();
							if(elementHeight!=null && elementHeight.intValue()>maxRowHeight[r]){
								maxRowHeight[r] = elementHeight.intValue();
							}
						}
					}
				}
			}
			
			int colHeight = borderTitleHeight ; //take into account if a border title has been added
			int[] absoluteRowHeight = new int[container.getRows()];
			for(int r= 0; r<container.getRows(); r++){
				colHeight = colHeight + maxRowHeight[r];
				absoluteRowHeight[r] = colHeight;
			}
			
			//and set top to be max height of previous row
			for (Container child : container.getContainers()) {
				child.setHeight(colHeight);
				
				for(ReportElement e: child.getElements()){
					//set element top to be the max height of the previous row
					final int elementRow = rowOf(e);
					if(elementRow>0){
						e.setElementTop(absoluteRowHeight[elementRow-1]);
					}
				}
			}			
			
			for(ReportElement e: band.getElements()){
				if(ElementType.border.equals(e.getElementType())){
					e.setElementHeight(colHeight);
				}
			}
		}
		
		// border box
		if (Boolean.TRUE.equals(container.getBorder())
				|| Boolean.TRUE.equals(design.getSectionBorderTop())
				|| Boolean.TRUE.equals(design.getSectionBorderLeft())
				|| Boolean.TRUE.equals(design.getSectionBorderBottom())
				|| Boolean.TRUE.equals(design.getSectionBorderRight())) {

			final Integer containerHeight = container.getHeight();
			if(containerHeight!=null && containerHeight.intValue()>0){
				// TODO implement borders as lines
				band = Renderer.addElement(band, ReportElement.ElementType.border, null, null, null, null, Integer.valueOf(0), container.getLeft(), container.getWidth(),
					containerHeight, null, null, null, null, null, null, null);
			}
		}

	}

	/**
	 * Returns the form row of an element in a form column - these are always given a row when they are added.
	 *
	 * @param e The element.
	 * @return The row.
	 */
	private static int rowOf(@Nonnull ReportElement e) {
		final Integer result = e.getRow();
		if (result == null) {
			throw new IllegalStateException("Report element " + e.getName() + " in a form column has no row");
		}
		return result.intValue();
	}

	/**
	 * Returns the height of a container that has been laid out - layout starts the height at 0.
	 *
	 * @param container The container.
	 * @return The height.
	 */
	private static int laidOutHeight(@Nonnull Container container) {
		final Integer result = container.getHeight();
		if (result == null) {
			throw new IllegalStateException("Container " + container.getContainerType() + " has no height after layout");
		}
		return result.intValue();
	}

	/**
	 * Creates a visitor that maps a Skyve view into report fields and bands.
	 *
	 * @param reportDesignGenerator Generator used for nested subreport designs.
	 * @param customer Customer metadata context.
	 * @param module Module metadata context.
	 * @param document Document metadata context.
	 * @param view View definition to traverse.
	 * @param uxui The UX/UI variant used to resolve the view.
	 */
	public ReportViewVisitor(@Nonnull ReportDesignGenerator reportDesignGenerator,
								@Nonnull CustomerImpl customer,
								@Nonnull ModuleImpl module,
								@Nonnull DocumentImpl document,
								@Nonnull ViewImpl view,
								@Nonnull String uxui) {
		super(customer, module, document, view, uxui);
		this.reportDesignGenerator = reportDesignGenerator;
		detailBands = new ArrayList<>();
		viewTitle = view.getLocalisedTitle();
	}

	/**
	 * Creates a report element from a view item
	 * 
	 * @param binding
	 * @param elementType
	 * @param pixelWidth
	 * @param percentageWidth
	 * @param responsiveWidth
	 */
	@SuppressWarnings({"java:S107", "boxing", "java:S3776"}) // Long parameter list preserves the existing framework/API contract; complexity OK.
	protected void addElementFromItem(@Nullable String binding, @Nonnull ReportElement.ElementType elementType, @Nullable Integer pixelWidth, @Nullable Integer percentageWidth, @Nullable Integer responsiveWidth, @Nullable Integer pixelHeight, 
			@Nullable String valueFontName, @Nullable String invisibleConditionName) {
		
		LOGGER.info("{}{}", binding, subreport);
		
		if (subreport) {
//			Module subModule = customer.getModule(sub.getModuleName());
//			Document subDocument = subModule.getDocument(customer, sub.getDocumentName());
//			
//			//special case of datagrid or list grid
//			ReportField fld = null;
//			switch (elementType) {
//			case border:
//				break;
//			case combo:
//			case checkBox:
//			case staticText:
//			case textField:
//				fld = Generator.fieldFromBinding(sub, customer, subDocument, binding);
//				sub.getFields().add(fld);
//				
//				break;
//			case colourPicker:
//				break;
//			case contentImage:
//				break;
//			case dynamicImage:
//				break;
//			case geometry:
//				break;
//			case html:
//				break;
//			case line:
//				break;
//			case radio:
//				break;
//			case richTextField:
//				break;
//			case slider:
//				break;
//			case spinner:
//				break;
//			case staticImage:
//				break;
//			case subreport:
//				break;
//			default:
//				break;
//			
//			}
			
		}
		else {

			ReportField fld = null;
			switch (elementType) {
			case border:
				break;
			case colourPicker:
				break;
			case checkBox:
			case combo:
			case contentImage:
			case staticText:
			case textField:
				if (binding != null) {
					fld = ReportDesignGenerator.fieldFromBinding(design, customer, document, binding);
				}

				String valueExpression = null;

				if (fld != null) {

					// Check form declaration is valid
					if (currentContainer.getContainers().isEmpty()
							|| currentContainer.getContainers().size() < col + 1
							|| (Boolean.TRUE.equals(showLabel) && currentContainer.getContainers().size() < col + 2)) {
						StringBuilder msg = new StringBuilder(64);
						msg.append("The view definition for ").append(design.getModuleName()).append('.').append(design.getDocumentName());
						msg.append(" near binding {").append(binding).append("} requires ");
						if (Boolean.TRUE.equals(showLabel)) {
							msg.append(col + 2);
						} else {
							msg.append(col + 1);
						}
						msg.append(" columns but only ").append(currentContainer.getContainers().size()).append(" have been declared.");
						throw new MetaDataException(msg.toString());
					}

					// add the field to the report field collection
					design.getFields().add(fld);

					// default expression for any field
					valueExpression = "$F{" + fld.getName() + "}";

					// special case for checkboxes - allow override of representation
					if (ReportElement.ElementType.checkBox.equals(elementType)) {
						if (design.getCheckBoxDisplayExpression().contains("{")) {
							valueExpression = design.getCheckBoxDisplayExpression().replace("<fieldName>", fld.getName());
						} else {
							valueExpression = design.getCheckBoxDisplayExpression().replace("<fieldName>", "$F{" + fld.getName() + "}");
						}
					}
					
					// add an implicit label element - this is different from a specified label item
					// implicit labels appear in the LHS column before the current column
					if (Boolean.TRUE.equals(showLabel)) {
						ReportElement.ElementType type = ReportElement.ElementType.staticText;
						String labelExpression = fld.getDisplayName();
						if (Boolean.TRUE.equals(design.getRenderLabelAsTextFields()) || Boolean.TRUE.equals(design.getDynamicFlow())) {
							type = ReportElement.ElementType.textField;
							labelExpression = "\"" + fld.getDisplayName() + "\"";
						}
						ReportElement.ElementAlignment align = ReportElement.ElementAlignment.right;
						if (design.getLabelAlignmentOverride() != null) {
							align = design.getLabelAlignmentOverride();
						} else if (labelAlignment != null) {
							align = labelAlignment;
						}
						ReportElement e = new ReportElement(type, fld.getName() + "_label", labelExpression, null, null, currentContainer.getVerticalPosition(), null,
								currentContainer.getContainers().get(col).getWidth(), null, null, align, design.getBoldLabels(), null, invisibleConditionName);
						e.setRow(row);
						// add the element to the previous sibling container
						currentContainer.getContainers().get(col).getElements().add(e);
						col++;
					}

					// add the field element
					if (ReportElement.ElementType.contentImage.equals(elementType)) {
						
						ReportElement e = new ReportElement(elementType, fld.getName() + "_contentImage", fld.getName(), currentContainer.getVerticalPosition(),
								currentContainer.getContainers().get(col).getWidth(), null, invisibleConditionName);
						e.setPixelWidth(pixelWidth);
						e.setPercentageWidth(percentageWidth);
						e.setResponsiveWidth(responsiveWidth);
						e.setElementHeight(pixelHeight);
						e.setRow(row);
						e.setField(fld);
						currentContainer.getContainers().get(col).getElements().add(e);
					} else {
						ReportElement e = new ReportElement(ReportElement.ElementType.textField, fld.getName() + "_field", valueExpression, valueFontName,
								design.getDefaultFontSize(), currentContainer.getVerticalPosition(), currentContainer.getContainers().get(col).getWidth(),
								null, null, null, itemAlignment, forceValueBold, forceValueItalic, invisibleConditionName);
						e.setPixelWidth(pixelWidth);
						e.setPercentageWidth(percentageWidth);
						e.setResponsiveWidth(responsiveWidth);
						e.setRow(row);
						e.setField(fld);
						currentContainer.getContainers().get(col).getElements().add(e);
					}
					col++;
				}

				break;
			case dynamicImage:
				ReportElement eD = new ReportElement(elementType, binding + "_dynamicImage", binding, null, null, null, invisibleConditionName);
				eD.setPixelWidth(pixelWidth);
				currentContainer.getElements().add(eD);
				break;
			case staticImage:
				ReportElement eS = new ReportElement(elementType, binding + "_staticImage", binding, null, null, null, invisibleConditionName);
				eS.setPixelWidth(pixelWidth);
				currentContainer.getElements().add(eS);
				break;
			case line:
			case subreport:
				if (binding != null) {
					fld = ReportDesignGenerator.fieldFromBinding(design, customer, document, binding);
				}
				design.getFields().add(fld);
				ReportElement s = new ReportElement(elementType, binding , binding, null, null, null, invisibleConditionName);
				s.setPixelWidth(pixelWidth);
				s.setPercentageWidth(percentageWidth);
				s.setResponsiveWidth(responsiveWidth);
				s.setField(fld);
				currentContainer.getElements().add(s);
				break;
			default:
				break;

			}

			// handle non field elements
		}
	}

	/**
	 * Convenience override for unsized items
	 * 
	 * @param binding
	 * @param type
	 */
	private void addElementFromItem(@Nullable String binding, @Nonnull ReportElement.ElementType type) {
		addElementFromItem(binding, type, null, null, null, null, null, null);
	}

	/**
	 * Visits a blurb widget.
	 */
	@Override
	public void visitBlurb(@Nonnull Blurb arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	/**
	 * Visits a button widget.
	 */
	@Override
	public void visitButton(@Nonnull Button arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	/**
	 * Visits a zoom-in widget.
	 */
	@Override
	public void visitZoomIn(@Nonnull ZoomIn arg0, boolean arg1, boolean arg2) {
		// Not supported
	}

	/**
	 * Visits a check box widget and emits a checkbox report element.
	 */
	@Override
	public void visitCheckBox(@Nonnull CheckBox arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ReportElement.ElementType.checkBox);
	}

	/**
	 * Visits a check-membership widget.
	 */
	@Override
	public void visitCheckMembership(@Nonnull CheckMembership arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	/**
	 * Visits a colour-picker widget and emits a colour report element.
	 */
	@Override
	public void visitColourPicker(@Nonnull ColourPicker arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ElementType.colourPicker, arg0.getPixelWidth(), null, null, null, null, arg0.getInvisibleCondition());	
	}

	/**
	 * Visits a combo widget and emits a combo report element.
	 */
	@Override
	public void visitCombo(@Nonnull Combo arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ElementType.combo);
	}

	/**
	 * Visits a comparison widget.
	 */
	@Override
	public void visitComparison(@Nonnull Comparison arg0, boolean arg1, boolean arg2) {
		// Not supported
	}

	/**
	 * Ignores managed-content uploads in report generation.
	 *
	 * @param content the content upload being visited
	 * @param parentVisible whether ancestor metadata is visible
	 * @param parentEnabled whether ancestor metadata is enabled
	 */
	@Override
	public void visitContent(@Nonnull ContentUpload content, boolean parentVisible, boolean parentEnabled) {
		// Not supported
	}

	/**
	 * Visits a content-signature widget and emits an image report element.
	 */
	@Override
	public void visitContentSignature(@Nonnull ContentSignature arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ReportElement.ElementType.contentImage, arg0.getPixelWidth(), null, null, arg0.getPixelHeight(), null, arg0.getInvisibleCondition());
	}

	/**
	 * Visits a data-grid widget.
	 */
	@Override
	public void visitDataGrid(@Nonnull DataGrid grid, boolean arg1, boolean arg2) {
		visitDataWidget(grid);
	}
	
	/**
	 * Visits a data-repeater widget.
	 */
	@Override
	public void visitDataRepeater(@Nonnull DataRepeater repeater, boolean arg1, boolean arg2) {
		visitDataWidget(repeater);
	}
	
	/**
	 * Visits a tabular data widget and emits a nested subreport element/design.
	 *
	 * <p>Side effects: appends fields, containers, and a subreport definition to
	 * the active design.
	 *
	 * @param widget The data widget to transform.
	 */
	public void visitDataWidget(@Nonnull AbstractDataWidget widget) {
		LOGGER.info("DATA GRID WITH BINDING{}", widget.getBinding());
		addContainer(widget.getWidgetId()
				, widget.getLocalisedTitle()
				, (widget.getTitle()==null?Boolean.FALSE: Boolean.TRUE)
				, widget.getPixelWidth()
				, widget.getPercentageWidth()
				, widget.getResponsiveWidth()
				, Boolean.FALSE
				, ContainerType.subreport
				, widget.getInvisibleCondition());
		
		
		// add a subreport element to the report
		addElementFromItem(widget.getBinding(), ReportElement.ElementType.subreport, widget.getPixelWidth(), widget.getPercentageWidth(), widget.getResponsiveWidth(), widget.getPixelHeight(), null, widget.getInvisibleCondition());

		//construct the subreport itself
		ReportElement subE  = currentContainer.getElements().get(currentContainer.getElements().size()-1);
		ReportField fld = subE.getField();
		if (fld == null) {
			throw new MetaDataException("The view definition for " + design.getModuleName() + '.' + design.getDocumentName() +
											" has a data widget with binding {" + widget.getBinding() + "} that cannot be reported on.");
		}
		
		sub = new DesignSpecification();
		sub.setDefinitionSource(DefinitionSource.document);
		sub.setReportType(ReportType.subreport);
		sub.setName(design.getName() + "_" + fld.getName());
		sub.setModuleName(fld.getOwningModuleName());
		sub.setDocumentName(fld.getDocumentName());

		sub.setWidth(design.getColumnWidth());
		sub.setColumnWidth(design.getColumnWidth());
		sub.setLeftMargin(Integer.valueOf(0)); // subreports normally don't have a margin as they sit inside another report
		sub.setRightMargin(Integer.valueOf(0)); // padding around subreports can be handled within the containing report
		sub.setTopMargin(Integer.valueOf(0));
		sub.setBottomMargin(Integer.valueOf(0));

		sub.setMode(design.getMode()); // default to same mode as containing report
		sub.setField(fld);
		sub.setParentReportPersistentName(Renderer.getPersistentIdentifierForDocument(document));
		sub.setRepositoryPath(design.getRepositoryPath());
		sub.setSaveToDocumentPackage(design.getSaveToDocumentPackage());

		// get the attribute and collection type
		for (Attribute attr : document.getAttributes()) {
			if (attr.getName().equals(fld.getName())) {
				Collection collection = (Collection) attr;
				sub.setCollectionType(CollectionType.valueOf(collection.getType().name()));
				break;
			}
		}

		ReportDesignGenerator subReportGenerator = reportDesignGenerator.getSubreportGenerator();
		try {
			subReportGenerator.populateDesign(sub);
		} catch (Exception e){
			LOGGER.info("COULD NOT CREATE DEFAULT DESIGN FOR SUB REPORT {}", sub.getName(), e);
		}
		
		design.getSubReports().add(sub);

		//Generator.constructSubreportFromField(subE.getField(), design.getColumnWidth());
		subE.setReportFileName(sub.getName());
		
		// create a subreport for the datagrid
		subreport = true;
	}

	/**
	 * Visits a data-grid bound column.
	 */
	@Override
	public void visitDataGridBoundColumn(@Nonnull DataGridBoundColumn arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	/**
	 * Visits a data-grid container column.
	 */
	@Override
	public void visitDataGridContainerColumn(@Nonnull DataGridContainerColumn arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	/**
	 * Visits a dialog button widget.
	 */
	@Override
	public void visitDialogButton(@Nonnull DialogButton arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	/**
	 * Visits a dynamic-image widget and emits a dynamic image report element.
	 */
	@Override
	public void visitDynamicImage(@Nonnull DynamicImage arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getName(), ReportElement.ElementType.dynamicImage, arg0.getPixelWidth(), arg0.getPercentageWidth(), arg0.getResponsiveWidth(), arg0.getPixelHeight(), null, arg0.getInvisibleCondition());

	}

	/**
	 * Visits a form container and initializes row/form traversal state.
	 */
	@Override
	public void visitForm(@Nonnull Form arg0, boolean arg1, boolean arg2) {
		addContainer(arg0.getWidgetId(), arg0.getBorderTitle(), arg0.getBorder(), arg0.getPixelWidth(), arg0.getPercentageWidth(), arg0.getResponsiveWidth(), Boolean.TRUE,
				ContainerType.form, arg0.getInvisibleCondition());
		row = 0;
		inForm = true;
	}

	/**
	 * Visits a form column and creates a column container.
	 */
	@Override
	public void visitFormColumn(@Nonnull FormColumn arg0, boolean arg1, boolean arg2) {
		addContainer(null, null, null, arg0.getPixelWidth(), arg0.getPercentageWidth(), arg0.getResponsiveWidth(), Boolean.FALSE, ContainerType.column, null);
		handleEndContainer();
	}

	/**
	 * Visits a form item and captures label/alignment rendering hints.
	 */
	@Override
	public void visitFormItem(@Nonnull FormItem arg0, boolean arg1, boolean arg2) {
		showLabel = (arg0.getShowLabel() == null ? Boolean.TRUE : arg0.getShowLabel());
		itemAlignment = ElementAlignment.fromHorizontalAlignment(arg0.getHorizontalAlignment());
		labelAlignment = ElementAlignment.fromHorizontalAlignment(arg0.getLabelHorizontalAlignment());
	}

	/**
	 * Visits a form row and resets row-local layout state.
	 */
	@Override
	public void visitFormRow(@Nonnull FormRow arg0, boolean arg1, boolean arg2) {
		// reset columns and starting left position
		col = 0;
		left = 0;
	}

	/**
	 * Visits a geometry widget and emits a geometry report element.
	 */
	@Override
	public void visitGeometry(@Nonnull Geometry arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ElementType.geometry, arg0.getPixelWidth(), null, null, null, null, arg0.getInvisibleCondition());
	}

	/**
	 * Completes processing for a geometry widget.
	 */
	@Override
	public void visitedGeometry(@Nonnull Geometry geometry, boolean parentVisible, boolean parentEnabled) {
		// No action required
	}
	
	/**
	 * Visits a geometry-map widget and emits a geometry report element.
	 */
	@Override
	public void visitGeometryMap(@Nonnull GeometryMap arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ElementType.geometry, arg0.getPixelWidth(), null, null, null, null, arg0.getInvisibleCondition());
	}

	/**
	 * Completes processing for a geometry-map widget.
	 */
	@Override
	public void visitedGeometryMap(@Nonnull GeometryMap geometry, boolean parentVisible, boolean parentEnabled) {
		// No action required
	}
	
	/**
	 * Visits an HBox container and emits a horizontal or vertical container based on verticalise settings.
	 */
	@Override
	public void visitHBox(@Nonnull HBox arg0, boolean arg1, boolean arg2) {
		Boolean horiz = Boolean.TRUE;
		if (Boolean.TRUE.equals(design.getVerticalise())) {
			horiz = Boolean.FALSE;
		}
		addContainer(arg0.getWidgetId(), arg0.getBorderTitle(), arg0.getBorder(), arg0.getPixelWidth(), arg0.getPercentageWidth(), arg0.getResponsiveWidth(), horiz, ContainerType.hbox, arg0.getInvisibleCondition());
	}

	/**
	 * Visits an HTML widget.
	 */
	@Override
	public void visitHTML(@Nonnull HTML arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	/**
	 * Visits an inject widget.
	 */
	@Override
	public void visitInject(@Nonnull Inject arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	/**
	 * Visits a label widget.
	 */
	@Override
	public void visitLabel(@Nonnull Label arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	/**
	 * Visits a link widget.
	 */
	@Override
	public void visitLink(@Nonnull Link arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	/**
	 * Visits a list-grid widget and marks subreport mode.
	 */
	@Override
	public void visitListGrid(@Nonnull ListGrid arg0, boolean arg1, boolean arg2) {
		subreport = true;
	}

	/**
	 * Visits a list-repeater widget and marks subreport mode.
	 */
	@Override
	public void visitListRepeater(@Nonnull ListRepeater repeater, boolean arg1, boolean arg2) {
		subreport = true;
	}

	/**
	 * Visits a list-membership widget and marks subreport mode.
	 */
	@Override
	public void visitListMembership(@Nonnull ListMembership arg0, boolean arg1, boolean arg2) {
		subreport = true;
	}

	/**
	 * Visits a lookup-description widget and emits a text-field report element.
	 */
	@Override
	public void visitLookupDescription(@Nonnull LookupDescription arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ElementType.textField);
	}

	/**
	 * Visits a map widget.
	 */
	@Override
	public void visitMap(@Nonnull MapDisplay arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	/**
	 * Visits a chart widget.
	 */
	@Override
	public void visitChart(@Nonnull Chart arg0, boolean arg1, boolean arg2) {
		// Not supported yet

	}

	/**
	 * Visits an on-added event handler.
	 */
	@Override
	public void visitOnAddedEventHandler(@Nonnull Addable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Visits an on-blur event handler.
	 */
	@Override
	public void visitOnBlurEventHandler(@Nonnull Focusable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Visits an on-changed event handler.
	 */
	@Override
	public void visitOnChangedEventHandler(@Nonnull Changeable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Visits an on-cleared event handler.
	 */
	@Override
	public void visitOnClearedEventHandler(@Nonnull LookupDescription arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Visits an on-edited event handler.
	 */
	@Override
	public void visitOnEditedEventHandler(@Nonnull Editable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Visits an on-focus event handler.
	 */
	@Override
	public void visitOnFocusEventHandler(@Nonnull Focusable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Visits an on-picked event handler.
	 */
	@Override
	public void visitOnPickedEventHandler(@Nonnull LookupDescription arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Visits an on-removed event handler.
	 */
	@Override
	public void visitOnRemovedEventHandler(@Nonnull Removable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Visits an on-selected event handler.
	 */
	@Override
	public void visitOnSelectedEventHandler(@Nonnull Selectable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Visits a password widget and emits a text-field report element.
	 */
	@Override
	public void visitPassword(@Nonnull Password arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ElementType.textField);
	}

	/**
	 * Visits a progress-bar widget.
	 */
	@Override
	public void visitProgressBar(@Nonnull ProgressBar arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Visits a radio widget and emits a radio report element.
	 */
	@Override
	public void visitRadio(@Nonnull Radio arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ElementType.radio, arg0.getPixelWidth(), null, null, null, null, arg0.getInvisibleCondition());
	}

	/**
	 * Visits a rerender event action.
	 */
	@Override
	public void visitRerenderEventAction(@Nonnull RerenderEventAction arg0, @Nonnull EventSource arg1, boolean arg2, boolean arg3) {
		// No action required

	}

	/**
	 * Visits a rich-text widget and emits a rich-text report element.
	 */
	@Override
	public void visitRichText(@Nonnull RichText arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ElementType.richTextField, arg0.getPixelWidth(), null, null, null, null, arg0.getInvisibleCondition());
	}

	/**
	 * Visits a server-side action event action.
	 */
	@Override
	public void visitServerSideActionEventAction(@Nonnull ServerSideActionEventAction arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Visits a set-disabled event action.
	 */
	@Override
	public void visitSetDisabledEventAction(@Nonnull SetDisabledEventAction arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Visits a set-invisible event action.
	 */
	@Override
	public void visitSetInvisibleEventAction(@Nonnull SetInvisibleEventAction arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Visits a slider widget and emits a slider report element.
	 */
	@Override
	public void visitSlider(@Nonnull Slider arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ElementType.slider, arg0.getPixelWidth(), null, null, null, null, arg0.getInvisibleCondition());	
	}

	/**
	 * Visits a spacer and advances the column position.
	 */
	@Override
	public void visitSpacer(@Nonnull Spacer arg0) {
		col++;
	}

	/**
	 * Visits a spinner widget and emits a spinner report element.
	 */
	@Override
	public void visitSpinner(@Nonnull Spinner arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ElementType.spinner, arg0.getPixelWidth(), null, null, null, null, arg0.getInvisibleCondition());
	}

	/**
	 * Visits a static-image widget and emits a static image report element.
	 */
	@Override
	public void visitStaticImage(@Nonnull StaticImage arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getRelativeFile(), ReportElement.ElementType.staticImage, arg0.getPixelWidth(), arg0.getPercentageWidth(), arg0.getResponsiveWidth(), arg0.getPixelHeight(), null, arg0.getInvisibleCondition());
	}

	/**
	 * Visits a tab container and emits a tab container section.
	 */
	@Override
	public void visitTab(@Nonnull Tab arg0, boolean arg1, boolean arg2) {
		addContainer(null, arg0.getLocalisedTitle(), Boolean.TRUE, null, null, null, Boolean.FALSE, ContainerType.tab, arg0.getInvisibleCondition());
	}

	/**
	 * Visits a tab-pane container.
	 */
	@Override
	public void visitTabPane(@Nonnull TabPane arg0, boolean arg1, boolean arg2) {
		// Ignored
	}

	/**
	 * Visits a text-area widget and emits a text-field report element.
	 */
	@Override
	public void visitTextArea(@Nonnull TextArea arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ReportElement.ElementType.textField, arg0.getPixelWidth(), null, null, null, null, arg0.getInvisibleCondition());
	}

	/**
	 * Visits a text-field widget and emits a text-field report element.
	 */
	@Override
	public void visitTextField(@Nonnull TextField arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ReportElement.ElementType.textField, arg0.getPixelWidth(), null, null, null, null, arg0.getInvisibleCondition());
	}

	/**
	 * Visits a toggle-disabled event action.
	 */
	@Override
	public void visitToggleDisabledEventAction(@Nonnull ToggleDisabledEventAction arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Visits a toggle-visibility event action.
	 */
	@Override
	public void visitToggleVisibilityEventAction(@Nonnull ToggleVisibilityEventAction arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Visits a tree-grid widget.
	 */
	@Override
	public void visitTreeGrid(@Nonnull TreeGrid arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	/**
	 * Visits a VBox container and emits a vertical container section.
	 */
	@Override
	public void visitVBox(@Nonnull VBox arg0, boolean arg1, boolean arg2) {
		addContainer(arg0.getWidgetId(),
						arg0.getBorderTitle(),
						arg0.getBorder(),
						arg0.getPixelWidth(),
						arg0.getPercentageWidth(),
						arg0.getResponsiveWidth(),
						Boolean.FALSE,
						ContainerType.vbox,
						arg0.getInvisibleCondition());
	}

	/**
	 * Visits the root view and marks traversal as complete.
	 */
	@Override
	public void visitView() {
		visited = true;
	}

	/**
	 * Completes processing for a check box widget.
	 */
	@Override
	public void visitedCheckBox(@Nonnull CheckBox arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Completes processing for a check-membership widget.
	 */
	@Override
	public void visitedCheckMembership(@Nonnull CheckMembership arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Completes processing for a colour-picker widget.
	 */
	@Override
	public void visitedColourPicker(@Nonnull ColourPicker arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Completes processing for a combo widget.
	 */
	@Override
	public void visitedCombo(@Nonnull Combo arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Completes processing for a data-grid widget and closes its container.
	 */
	@Override
	public void visitedDataGrid(@Nonnull DataGrid arg0, boolean arg1, boolean arg2) {
		subreport = false;
		handleEndContainer();
	}

	/**
	 * Completes processing for a data-repeater widget and closes its container.
	 */
	@Override
	public void visitedDataRepeater(@Nonnull DataRepeater repeater, boolean arg1, boolean arg2) {
		subreport = false;
		handleEndContainer();
	}

	/**
	 * Completes processing for a data-grid bound column.
	 */
	@Override
	public void visitedDataGridBoundColumn(@Nonnull DataGridBoundColumn arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Completes processing for a data-grid container column and closes its container.
	 */
	@Override
	public void visitedDataGridContainerColumn(@Nonnull DataGridContainerColumn arg0, boolean arg1, boolean arg2) {
		handleEndContainer();
	}

	/**
	 * Completes processing for a form container and finalizes column layout state.
	 */
	@Override
	public void visitedForm(@Nonnull Form arg0, boolean arg1, boolean arg2) {
		inForm = false;

		// remove any trailing columns which are empty
		// they can't be placeholders if they are to the right as rows are filled from left to right
		// TODO handle internationalisation direction
		int colCount = currentContainer.getContainers().size();
		for (int i = colCount - 1; i >= 0; i--) {
			if (!currentContainer.getContainers().get(i).getElements().isEmpty()) {
				break;
			}
			// remove this column
			currentContainer.getContainers().remove(i);
		}

		// set the total rows for all columns 
		for (Container container : currentContainer.getContainers()) {
			container.setRows(currentContainer.getRows());
		}
		
		handleEndContainer();
	}

	/**
	 * Completes processing for a form item.
	 */
	@Override
	public void visitedFormItem(@Nonnull FormItem arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Completes processing for a form row and advances row count when the row has content.
	 */
	@Override
	public void visitedFormRow(@Nonnull FormRow arg0, boolean arg1, boolean arg2) {
		// ignore row if all columns for this row are empty
		boolean allEmpty = true;
		for (Container container : currentContainer.getContainers()) {
			for (ReportElement e : container.getElements()) {
				if (rowOf(e) == row) {
					allEmpty = false;
					break;
				}
			}
		}
		if (!allEmpty) {
			row++;
			currentContainer.setRows(row);
		}
	}

	/**
	 * Completes processing for an HBox container and closes its nesting level.
	 */
	@Override
	public void visitedHBox(@Nonnull HBox arg0, boolean arg1, boolean arg2) {
		handleEndContainer();
	}

	/**
	 * Completes processing for a list-grid widget.
	 */
	@Override
	public void visitedListGrid(@Nonnull ListGrid arg0, boolean arg1, boolean arg2) {
		subreport = false;
	}

	/**
	 * Completes processing for a list-repeater widget.
	 */
	@Override
	public void visitedListRepeater(@Nonnull ListRepeater repeater, boolean arg1, boolean arg2) {
		subreport = false;
	}

	/**
	 * Completes processing for a list-membership widget.
	 */
	@Override
	public void visitedListMembership(@Nonnull ListMembership arg0, boolean arg1, boolean arg2) {
		subreport = false;
	}

	/**
	 * Completes processing for a lookup-description widget.
	 */
	@Override
	public void visitedLookupDescription(@Nonnull LookupDescription arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Completes processing for an on-added event handler.
	 */
	@Override
	public void visitedOnAddedEventHandler(@Nonnull Addable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Completes processing for an on-blur event handler.
	 */
	@Override
	public void visitedOnBlurEventHandler(@Nonnull Focusable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Completes processing for an on-changed event handler.
	 */
	@Override
	public void visitedOnChangedEventHandler(@Nonnull Changeable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Completes processing for an on-cleared event handler.
	 */
	@Override
	public void visitedOnClearedEventHandler(@Nonnull LookupDescription arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Completes processing for an on-edited event handler.
	 */
	@Override
	public void visitedOnEditedEventHandler(@Nonnull Editable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Completes processing for an on-focus event handler.
	 */
	@Override
	public void visitedOnFocusEventHandler(@Nonnull Focusable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Completes processing for an on-picked event handler.
	 */
	@Override
	public void visitedOnPickedEventHandler(@Nonnull LookupDescription arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Completes processing for an on-removed event handler.
	 */
	@Override
	public void visitedOnRemovedEventHandler(@Nonnull Removable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Completes processing for an on-selected event handler.
	 */
	@Override
	public void visitedOnSelectedEventHandler(@Nonnull Selectable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Completes processing for a password widget.
	 */
	@Override
	public void visitedPassword(@Nonnull Password arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Completes processing for a radio widget.
	 */
	@Override
	public void visitedRadio(@Nonnull Radio arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Completes processing for a rich-text widget.
	 */
	@Override
	public void visitedRichText(@Nonnull RichText arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Completes processing for a slider widget.
	 */
	@Override
	public void visitedSlider(@Nonnull Slider arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Completes processing for a spinner widget.
	 */
	@Override
	public void visitedSpinner(@Nonnull Spinner arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Completes processing for a tab container and closes its nesting level.
	 */
	@Override
	public void visitedTab(@Nonnull Tab arg0, boolean arg1, boolean arg2) {
		handleEndContainer();
	}

	/**
	 * Completes processing for a tab-pane container.
	 */
	@Override
	public void visitedTabPane(@Nonnull TabPane arg0, boolean arg1, boolean arg2) {
		// handleEndContainer();
	}

	/**
	 * Completes processing for a text-area widget.
	 */
	@Override
	public void visitedTextArea(@Nonnull TextArea arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Completes processing for a text-field widget.
	 */
	@Override
	public void visitedTextField(@Nonnull TextField arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Completes processing for a tree-grid widget.
	 */
	@Override
	public void visitedTreeGrid(@Nonnull TreeGrid arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Completes processing for a VBox container and closes its nesting level.
	 */
	@Override
	public void visitedVBox(@Nonnull VBox arg0, boolean arg1, boolean arg2) {
		handleEndContainer();
	}

	/**
	 * Completes processing for the root view.
	 */
	@Override
	public void visitedView() {
		// No action required

	}

	/**
	 * Visits an add action.
	 */
	@Override
	public void visitAddAction(@Nonnull ActionImpl arg0) {
		// No action required

	}

	/**
	 * Visits a biz-export action.
	 */
	@Override
	public void visitBizExportAction(@Nonnull ActionImpl arg0) {
		// No action required

	}

	/**
	 * Visits a biz-import action.
	 */
	@Override
	public void visitBizImportAction(@Nonnull ActionImpl arg0) {
		// No action required

	}

	/**
	 * Visits a cancel action.
	 */
	@Override
	public void visitCancelAction(@Nonnull ActionImpl arg0) {
		// No action required

	}

	/**
	 * Visits a custom action.
	 */
	@Override
	public void visitCustomAction(@Nonnull ActionImpl arg0) {
		// No action required

	}

	/**
	 * Visits a delete action.
	 */
	@Override
	public void visitDeleteAction(@Nonnull ActionImpl arg0) {
		// No action required

	}

	/**
	 * Visits a download action.
	 */
	@Override
	public void visitDownloadAction(@Nonnull ActionImpl arg0) {
		// No action required

	}

	/**
	 * Visits an edit action.
	 */
	@Override
	public void visitEditAction(@Nonnull ActionImpl arg0) {
		// No action required

	}

	/**
	 * Visits a filter parameter.
	 */
	@Override
	public void visitFilterParameter(@Nonnull FilterParameter arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Visits a navigate action.
	 */
	@Override
	public void visitNavigateAction(@Nonnull ActionImpl arg0) {
		// No action required

	}

	/**
	 * Visits a new action.
	 */
	@Override
	public void visitNewAction(@Nonnull ActionImpl arg0) {
		// No action required

	}

	/**
	 * Visits an OK action.
	 */
	@Override
	public void visitOKAction(@Nonnull ActionImpl arg0) {
		// No action required

	}

	/**
	 * Visits an action parameter.
	 */
	@Override
	public void visitParameter(@Nonnull Parameter arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	/**
	 * Visits a remove action.
	 */
	@Override
	public void visitRemoveAction(@Nonnull ActionImpl arg0) {
		// No action required

	}

	/**
	 * Visits a report action.
	 */
	@Override
	public void visitReportAction(@Nonnull ActionImpl arg0) {
		// No action required

	}

	/**
	 * Visits a save action.
	 */
	@Override
	public void visitSaveAction(@Nonnull ActionImpl arg0) {
		// No action required

	}

	/**
	 * Visits an upload action.
	 */
	@Override
	public void visitUploadAction(@Nonnull ActionImpl arg0) {
		// No action required

	}

	/**
	 * Visits a zoom-out action.
	 */
	@Override
	public void visitZoomOutAction(@Nonnull ActionImpl arg0) {
		// No action required

	}

	/**
	 * Visits a sidebar container.
	 */
	@Override
	public void visitSidebar(@Nonnull Sidebar sidebar, boolean parentVisible, boolean parentEnabled) {
		// Not supported
	}

	/**
	 * Completes processing for a sidebar container.
	 */
	@Override
	public void visitedSidebar(@Nonnull Sidebar sidebar, boolean parentVisible, boolean parentEnabled) {
		// Not supported
	}
}
