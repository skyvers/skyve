package org.skyve.impl.generate.jasperreports;

import java.util.ArrayList;
import java.util.List;

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
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.Comparison;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
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
import org.skyve.util.Util;

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

	public boolean isVisited() {
		return visited;
	}

	public void setVisited(boolean visited) {
		this.visited = visited;
	}

	public String getViewTitle() {
		return viewTitle;
	}

	public Boolean getShowLabel() {
		return showLabel;
	}

	public void setShowLabel(Boolean showLabel) {
		this.showLabel = showLabel;
	}

	/**
	 * Set the report design which dictates how to interpret the view
	 * 
	 * @param reportDesign
	 */
	public void setDesign(DesignSpecification reportDesign) {
		design = reportDesign;
	}


	/**
	 * After visiting, return all report bands, according to the design provided
	 * 
	 * @return
	 */
	public List<ReportBand> getDetailBands() {
		return detailBands;
	}

	/**
	 * Calculate a size in TWIPS, given either a pixel, percentage or responsive specification
	 * 
	 * Priority is given in order of specificity - Pixel, Percentage and Responsive
	 * 
	 * TODO: handle selection of a specific uxui - responsive may be a higher priority than percentage
	 * 
	 * @param containerSizeInTwips
	 * @param pixels
	 * @param percentage
	 * @param responsive
	 * @return
	 */
	@SuppressWarnings("boxing")
	private Integer calculateTwipSize(Integer containerSizeInTwips, Integer pixels, Integer percentage, Integer responsive) {
		Integer resultSizeInTwips = null; // auto set
		if (pixels != null) {
			if (design.getPixelToTwip() != null) {
				resultSizeInTwips = Double.valueOf(pixels.doubleValue() * design.getPixelToTwip().doubleValue()).intValue();
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
	protected void startDetailBand(String widgetId, String title, String invisibleConditionName) {

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
			Util.LOGGER.warning("COULD NOT CONSTRUCT BAND " + detailBands.size() + " FOR WIDGET_ID " + widgetId);
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
	protected void addContainer(String widgetId, String borderTitle, Boolean border, Integer pixelWidth, Integer percentageWidth, Integer responsiveWidth, Boolean horizontal,
			ContainerType type, String invisibleConditionName) {

		if (currentContainer != null
				&& (Boolean.TRUE.equals(design.getVerticalise()))
				&& !inForm) {
			handleEndContainer();
		}

		if (currentContainer == null) {
			currentContainer = new Container(top, left, null, horizontal, type);
			currentContainer.setPixelWidth(pixelWidth);
			currentContainer.setPercentageWidth(percentageWidth);
			currentContainer.setResponsiveWidth(responsiveWidth);
			currentContainer.setBorder(border);
			currentContainer.setBorderTitle(borderTitle);

			if (containerDepth == 0 || Boolean.TRUE.equals(design.getVerticalise()) && !inForm) {
				startDetailBand(widgetId, borderTitle, invisibleConditionName);
			}

		} else {
			Container container = new Container(top, left, null, horizontal, type);
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
	@SuppressWarnings("boxing")
	protected void layout(Container container) {
		
		int sizedCols = 0;
		int borderTitleHeight  = 0;

		// calculate a specified width if one is supplied, otherwise stretch to fill container width
		Integer parentWidth;
		if (container.getParent() == null || container.getParent().getWidth() == null) {
			parentWidth = design.getColumnWidth();
		} else {
			parentWidth = container.getParent().getWidth();
		}
		// in Verticalise mode - stretch horizontally
		if (!Boolean.TRUE.equals(design.getVerticalise()) || inForm) {
			Integer calcWidth = calculateTwipSize(parentWidth, container.getPixelWidth(), container.getPercentageWidth(), container.getResponsiveWidth());
			if (calcWidth != null) {
				container.setWidth(calcWidth);
			}
		}
		if (container.getWidth() == null) {
			container.setWidth(parentWidth);
		}
		int remainingWidth = container.getWidth();

		container.setHeight(Integer.valueOf(0));
		container.setVerticalPosition(container.getTop());

		// BORDER TITLE
		// Border Title, (border done once height is calculated)
		if (Boolean.TRUE.equals(container.getBorder())) {

			if (container.getBorderTitle() != null) {

				borderTitleHeight = design.getDefaultElementHeight();
				band = Renderer.addElement(band, ReportElement.ElementType.staticText, container.getContainerType().name() + "_title", container.getBorderTitle(), null, null,
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

						if (e.getRow().intValue() == currRow) {

							// this container has something inside it
							found = true;
							container.setFilled(true);

							e.setElementWidth(calculateTwipSize(container.getWidth(), e.getPixelWidth(), e.getPercentageWidth(), e.getResponsiveWidth()));

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
							if (e.getElementHeight() == null) {
								e.setElementHeight(design.getDefaultElementHeight());
							}

							// update container height
							container.setVerticalPosition(container.getVerticalPosition() + e.getElementHeight());
							container.addHeight(e.getElementHeight());
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
						e.setElementWidth(calculateTwipSize(container.getWidth(), e.getPixelWidth(), e.getPercentageWidth(), e.getResponsiveWidth()));
						remainingWidth = remainingWidth - e.getElementWidth().intValue();
						sizedElems++;
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
					if (e.getElementWidth() == null) {
						if (container.getElements().indexOf(e) == container.getElements().size() - 1) {
							e.setElementWidth(remainingWidth);
						} else {
							e.setElementWidth(blockSize);
						}
					}

					// set height if not already specified
					// TODO - calculate height if specified - default to square shapes
					if(ElementType.subreport.equals(e.getElementType())){
						e.setElementHeight(design.getDefaultElementHeight());
					} 
					else {
						if (e.getElementHeight() == null) {
							e.setElementHeight(e.getElementWidth());
						}
					}

					if (Boolean.TRUE.equals(container.getHorizontal())) {
						// update container width
						container.setLeft(container.getLeft() + e.getElementWidth());
						remainingWidth = remainingWidth - blockSize;
					} else {
						// update container height
						container.setVerticalPosition(container.getVerticalPosition() + e.getElementHeight());
						container.addHeight(e.getElementHeight());
					}
				}

			}
		} else {

			// Layout horizontal spaced items
			if (Boolean.TRUE.equals(container.getHorizontal())) {

				for (Container c : container.getContainers()) {

					// ignore unsized columns to calculate total of sized columns
					Integer colW = calculateTwipSize(container.getWidth(), c.getPixelWidth(), c.getPercentageWidth(), c.getResponsiveWidth());
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
			int horizontalPosition = container.getLeft();
			int verticalPosition = container.getVerticalPosition();
			int maxHeight = 0;
			for (Container c : container.getContainers()) {

				// set absolute position by calculating effect of previous containers
				if (Boolean.TRUE.equals(container.getHorizontal()) && index > 0) {
					horizontalPosition = horizontalPosition + container.getContainers().get(index - 1).getWidth();
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
					if (c.getHeight().intValue() > maxHeight) {
						maxHeight = c.getHeight();
					}
				} else {
					// size vertical
					// Todo cleanup
					if (c.getHeight() == null) {
						c.setHeight(Integer.valueOf(0));
					}
					container.addHeight(c.getHeight());
					container.setVerticalPosition(container.getHeight());
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
				container.setVerticalPosition(container.getHeight());
			}

		}
		
		//fix row heights for form
		if(ContainerType.form.equals(container.getContainerType())){
			
			//set max row height
			int[] maxRowHeight = new int[container.getRows()];
			for(int r= 0; r<container.getRows(); r++){
				maxRowHeight[r] = 0;
				for (Container col : container.getContainers()) {
					for(ReportElement e: col.getElements()){
						if(e.getRow().intValue()==r){
							if(e.getElementHeight()!=null && e.getElementHeight().intValue()>maxRowHeight[r]){
								maxRowHeight[r] = e.getElementHeight().intValue();
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
			for (Container col : container.getContainers()) {
				col.setHeight(colHeight);
				
				for(ReportElement e: col.getElements()){
					//set element top to be the max height of the previous row
					if(e.getRow().intValue()>0){
						e.setElementTop(absoluteRowHeight[e.getRow().intValue()-1]);
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

			if(container.getHeight()!=null && container.getHeight().intValue()>0){
				// TODO implement borders as lines
				band = Renderer.addElement(band, ReportElement.ElementType.border, null, null, null, null, Integer.valueOf(0), container.getLeft(), container.getWidth(),
					container.getHeight(), null, null, null, null, null, null, null);
			}
		}

	}

	/**
	 * Constructor for this visitor - instantiate result lists for fields and bands
	 * 
	 * @param customer
	 * @param module
	 * @param document
	 * @param view
	 */
	public ReportViewVisitor(ReportDesignGenerator reportDesignGenerator, CustomerImpl customer, ModuleImpl module, DocumentImpl document, ViewImpl view) {
		super(customer, module, document, view);
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
	@SuppressWarnings("boxing")
	protected void addElementFromItem(String binding, ReportElement.ElementType elementType, Integer pixelWidth, Integer percentageWidth, Integer responsiveWidth, Integer pixelHeight, 
			String valueFontName, String invisibleConditionName) {
		
		Util.LOGGER.info(binding + subreport);
		
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
					fld = reportDesignGenerator.fieldFromBinding(design, customer, document, binding);
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
					if (ReportElement.ElementType.checkBox.equals(elementType) && design.getCheckBoxDisplayExpression() != null) {
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
				fld = reportDesignGenerator.fieldFromBinding(design, customer, document, binding);
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
	private void addElementFromItem(String binding, ReportElement.ElementType type) {
		addElementFromItem(binding, type, null, null, null, null, null, null);
	}

	@Override
	public void visitBlurb(Blurb arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	@Override
	public void visitButton(Button arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	@Override
	public void visitCheckBox(CheckBox arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ReportElement.ElementType.checkBox);
	}

	@Override
	public void visitCheckMembership(CheckMembership arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	@Override
	public void visitColourPicker(ColourPicker arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ElementType.colourPicker, arg0.getPixelWidth(), null, null, null, null, arg0.getInvisibleConditionName());	
	}

	@Override
	public void visitCombo(Combo arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ElementType.combo);
	}

	@Override
	public void visitComparison(Comparison arg0, boolean arg1, boolean arg2) {
		// Not supported
	}

	@Override
	public void visitContentImage(ContentImage arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ReportElement.ElementType.contentImage, arg0.getPixelWidth(), arg0.getPercentageWidth(), arg0.getResponsiveWidth(), arg0.getPixelHeight(), null, arg0.getInvisibleConditionName());
	}

	@Override
	public void visitContentLink(ContentLink arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	@Override
	public void visitContentSignature(ContentSignature arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ReportElement.ElementType.contentImage, arg0.getPixelWidth(), null, null, arg0.getPixelHeight(), null, arg0.getInvisibleConditionName());
	}

	@Override
	public void visitDataGrid(DataGrid grid, boolean arg1, boolean arg2) {
		visitDataWidget(grid);
	}
	
	@Override
	public void visitDataRepeater(DataRepeater repeater, boolean arg1, boolean arg2) {
		visitDataWidget(repeater);
	}
	
	public void visitDataWidget(AbstractDataWidget widget) {
		Util.LOGGER.info("DATA GRID WITH BINDING" + widget.getBinding());
		addContainer(widget.getWidgetId()
				, widget.getLocalisedTitle()
				, (widget.getTitle()==null?Boolean.FALSE: Boolean.TRUE)
				, widget.getPixelWidth()
				, widget.getPercentageWidth()
				, widget.getResponsiveWidth()
				, Boolean.FALSE
				, ContainerType.subreport
				, widget.getInvisibleConditionName());
		
		
		// add a subreport element to the report
		addElementFromItem(widget.getBinding(), ReportElement.ElementType.subreport, widget.getPixelWidth(), widget.getPercentageWidth(), widget.getResponsiveWidth(), widget.getPixelHeight(), null, widget.getInvisibleConditionName());

		//construct the subreport itself
		ReportElement subE  = currentContainer.getElements().get(currentContainer.getElements().size()-1);
		ReportField fld = subE.getField();
		
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
		sub.setParentReportPersistentName(Renderer.getPersistentFromDocument(document));
		sub.setRepositoryPath(design.getRepositoryPath());
		sub.setSaveToDocumentPackage(design.getSaveToDocumentPackage());

		// get the attribute and collection type
		for (Attribute attr : document.getAttributes()) {
			if (attr.getName().equals(fld.getName())) {
				Collection col = (Collection) attr;
				sub.setCollectionType(CollectionType.valueOf(col.getType().name()));
				break;
			}
		}

		ReportDesignGenerator subReportGenerator = reportDesignGenerator.getSubreportGenerator();
		try {
			subReportGenerator.populateDesign(sub);
		} catch (Exception e){
			Util.LOGGER.info("COULD NOT CREATE DEFAULT DESIGN FOR SUB REPORT " + sub.getName());
		}
		
		design.getSubReports().add(sub);

		//Generator.constructSubreportFromField(subE.getField(), design.getColumnWidth());
		subE.setReportFileName(sub.getName());
		
		// create a subreport for the datagrid
		subreport = true;
	}

	@Override
	public void visitDataGridBoundColumn(DataGridBoundColumn arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	@Override
	public void visitDataGridContainerColumn(DataGridContainerColumn arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	@Override
	public void visitDialogButton(DialogButton arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	@Override
	public void visitDynamicImage(DynamicImage arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getName(), ReportElement.ElementType.dynamicImage, arg0.getPixelWidth(), arg0.getPercentageWidth(), arg0.getResponsiveWidth(), arg0.getPixelHeight(), null, arg0.getInvisibleConditionName());

	}

	@Override
	public void visitForm(Form arg0, boolean arg1, boolean arg2) {
		addContainer(arg0.getWidgetId(), arg0.getBorderTitle(), arg0.getBorder(), arg0.getPixelWidth(), arg0.getPercentageWidth(), arg0.getResponsiveWidth(), Boolean.TRUE,
				ContainerType.form, arg0.getInvisibleConditionName());
		row = 0;
		inForm = true;
	}

	@Override
	public void visitFormColumn(FormColumn arg0, boolean arg1, boolean arg2) {
		addContainer(null, null, null, arg0.getPixelWidth(), arg0.getPercentageWidth(), arg0.getResponsiveWidth(), Boolean.FALSE, ContainerType.column, null);
		handleEndContainer();
	}

	@Override
	public void visitFormItem(FormItem arg0, boolean arg1, boolean arg2) {
		showLabel = (arg0.getShowLabel() == null ? Boolean.TRUE : arg0.getShowLabel());
		itemAlignment = ElementAlignment.fromHorizontalAlignment(arg0.getHorizontalAlignment());
		labelAlignment = ElementAlignment.fromHorizontalAlignment(arg0.getLabelHorizontalAlignment());
	}

	@Override
	public void visitFormRow(FormRow arg0, boolean arg1, boolean arg2) {
		// reset columns and starting left position
		col = 0;
		left = 0;
	}

	@Override
	public void visitGeometry(Geometry arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ElementType.geometry, arg0.getPixelWidth(), null, null, null, null, arg0.getInvisibleConditionName());
	}

	@Override
	public void visitedGeometry(Geometry geometry, boolean parentVisible, boolean parentEnabled) {
		// No action required
	}
	
	@Override
	public void visitGeometryMap(GeometryMap arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ElementType.geometry, arg0.getPixelWidth(), null, null, null, null, arg0.getInvisibleConditionName());
	}

	@Override
	public void visitedGeometryMap(GeometryMap geometry, boolean parentVisible, boolean parentEnabled) {
		// No action required
	}
	
	@Override
	public void visitHBox(HBox arg0, boolean arg1, boolean arg2) {
		Boolean horiz = Boolean.TRUE;
		if (Boolean.TRUE.equals(design.getVerticalise())) {
			horiz = Boolean.FALSE;
		}
		addContainer(arg0.getWidgetId(), arg0.getBorderTitle(), arg0.getBorder(), arg0.getPixelWidth(), arg0.getPercentageWidth(), arg0.getResponsiveWidth(), horiz, ContainerType.hbox, arg0.getInvisibleConditionName());
	}

	@Override
	public void visitHTML(HTML arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	@Override
	public void visitInject(Inject arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	@Override
	public void visitLabel(Label arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	@Override
	public void visitLink(Link arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	@Override
	public void visitListGrid(ListGrid arg0, boolean arg1, boolean arg2) {
		subreport = true;
	}

	@Override
	public void visitListRepeater(ListRepeater repeater, boolean arg1, boolean arg2) {
		subreport = true;
	}

	@Override
	public void visitListMembership(ListMembership arg0, boolean arg1, boolean arg2) {
		subreport = true;
	}

	@Override
	public void visitLookupDescription(LookupDescription arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ElementType.textField);
	}

	@Override
	public void visitMap(MapDisplay arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	@Override
	public void visitChart(Chart arg0, boolean arg1, boolean arg2) {
		// Not supported yet

	}

	@Override
	public void visitOnAddedEventHandler(Addable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitOnBlurEventHandler(Focusable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitOnChangedEventHandler(Changeable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitOnClearedEventHandler(LookupDescription arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitOnEditedEventHandler(Editable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitOnFocusEventHandler(Focusable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitOnPickedEventHandler(LookupDescription arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitOnRemovedEventHandler(Removable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitOnSelectedEventHandler(Selectable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitPassword(Password arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ElementType.textField);
	}

	@Override
	public void visitProgressBar(ProgressBar arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitRadio(Radio arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ElementType.radio, arg0.getPixelWidth(), null, null, null, null, arg0.getInvisibleConditionName());
	}

	@Override
	public void visitRerenderEventAction(RerenderEventAction arg0, EventSource arg1, boolean arg2, boolean arg3) {
		// No action required

	}

	@Override
	public void visitRichText(RichText arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ElementType.richTextField, arg0.getPixelWidth(), null, null, null, null, arg0.getInvisibleConditionName());
	}

	@Override
	public void visitServerSideActionEventAction(ServerSideActionEventAction arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitSetDisabledEventAction(SetDisabledEventAction arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitSetInvisibleEventAction(SetInvisibleEventAction arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitSlider(Slider arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ElementType.slider, arg0.getPixelWidth(), null, null, null, null, arg0.getInvisibleConditionName());	
	}

	@Override
	public void visitSpacer(Spacer arg0) {
		col++;
	}

	@Override
	public void visitSpinner(Spinner arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ElementType.spinner, arg0.getPixelWidth(), null, null, null, null, arg0.getInvisibleConditionName());
	}

	@Override
	public void visitStaticImage(StaticImage arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getRelativeFile(), ReportElement.ElementType.staticImage, arg0.getPixelWidth(), arg0.getPercentageWidth(), arg0.getResponsiveWidth(), arg0.getPixelHeight(), null, arg0.getInvisibleConditionName());
	}

	@Override
	public void visitTab(Tab arg0, boolean arg1, boolean arg2) {
		addContainer(null, arg0.getLocalisedTitle(), Boolean.TRUE, null, null, null, Boolean.FALSE, ContainerType.tab, arg0.getInvisibleConditionName());
	}

	@Override
	public void visitTabPane(TabPane arg0, boolean arg1, boolean arg2) {
		// Ignored
	}

	@Override
	public void visitTextArea(TextArea arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ReportElement.ElementType.textField, arg0.getPixelWidth(), null, null, null, null, arg0.getInvisibleConditionName());
	}

	@Override
	public void visitTextField(TextField arg0, boolean arg1, boolean arg2) {
		addElementFromItem(arg0.getBinding(), ReportElement.ElementType.textField, arg0.getPixelWidth(), null, null, null, null, arg0.getInvisibleConditionName());
	}

	@Override
	public void visitToggleDisabledEventAction(ToggleDisabledEventAction arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitToggleVisibilityEventAction(ToggleVisibilityEventAction arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitTreeGrid(TreeGrid arg0, boolean arg1, boolean arg2) {
		// Not supported

	}

	@Override
	public void visitVBox(VBox arg0, boolean arg1, boolean arg2) {
		addContainer(arg0.getWidgetId(), arg0.getBorderTitle(), arg0.getBorder(), arg0.getPixelWidth(), arg0.getPercentageWidth(), arg0.getResponsiveWidth(), Boolean.FALSE,
				ContainerType.vbox, arg0.getInvisibleConditionName());
	}

	@Override
	public void visitView() {
		visited = true;
	}

	@Override
	public void visitedCheckBox(CheckBox arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitedCheckMembership(CheckMembership arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitedColourPicker(ColourPicker arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitedCombo(Combo arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitedDataGrid(DataGrid arg0, boolean arg1, boolean arg2) {
		subreport = false;
		handleEndContainer();
	}

	@Override
	public void visitedDataRepeater(DataRepeater repeater, boolean arg1, boolean arg2) {
		subreport = false;
		handleEndContainer();
	}

	@Override
	public void visitedDataGridBoundColumn(DataGridBoundColumn arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitedDataGridContainerColumn(DataGridContainerColumn arg0, boolean arg1, boolean arg2) {
		handleEndContainer();
	}

	@Override
	public void visitedForm(Form arg0, boolean arg1, boolean arg2) {
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
		for (Container col : currentContainer.getContainers()) {
			col.setRows(currentContainer.getRows());
		}
		
		handleEndContainer();
	}

	@Override
	public void visitedFormItem(FormItem arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitedFormRow(FormRow arg0, boolean arg1, boolean arg2) {
		// ignore row if all columns for this row are empty
		boolean allEmpty = true;
		for (Container col : currentContainer.getContainers()) {
			for (ReportElement e : col.getElements()) {
				if (e.getRow().equals(row)) {
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

	@Override
	public void visitedHBox(HBox arg0, boolean arg1, boolean arg2) {
		handleEndContainer();
	}

	@Override
	public void visitedListGrid(ListGrid arg0, boolean arg1, boolean arg2) {
		subreport = false;
	}

	@Override
	public void visitedListRepeater(ListRepeater repeater, boolean arg1, boolean arg2) {
		subreport = false;
	}

	@Override
	public void visitedListMembership(ListMembership arg0, boolean arg1, boolean arg2) {
		subreport = false;
	}

	@Override
	public void visitedLookupDescription(LookupDescription arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitedOnAddedEventHandler(Addable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitedOnBlurEventHandler(Focusable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitedOnChangedEventHandler(Changeable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitedOnClearedEventHandler(LookupDescription arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitedOnEditedEventHandler(Editable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitedOnFocusEventHandler(Focusable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitedOnPickedEventHandler(LookupDescription arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitedOnRemovedEventHandler(Removable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitedOnSelectedEventHandler(Selectable arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitedPassword(Password arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitedRadio(Radio arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitedRichText(RichText arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitedSlider(Slider arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitedSpinner(Spinner arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitedTab(Tab arg0, boolean arg1, boolean arg2) {
		handleEndContainer();
	}

	@Override
	public void visitedTabPane(TabPane arg0, boolean arg1, boolean arg2) {
		// handleEndContainer();
	}

	@Override
	public void visitedTextArea(TextArea arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitedTextField(TextField arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitedTreeGrid(TreeGrid arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitedVBox(VBox arg0, boolean arg1, boolean arg2) {
		handleEndContainer();
	}

	@Override
	public void visitedView() {
		// No action required

	}

	@Override
	public void visitAddAction(ActionImpl arg0) {
		// No action required

	}

	@Override
	public void visitBizExportAction(ActionImpl arg0) {
		// No action required

	}

	@Override
	public void visitBizImportAction(ActionImpl arg0) {
		// No action required

	}

	@Override
	public void visitCancelAction(ActionImpl arg0) {
		// No action required

	}

	@Override
	public void visitCustomAction(ActionImpl arg0) {
		// No action required

	}

	@Override
	public void visitDeleteAction(ActionImpl arg0) {
		// No action required

	}

	@Override
	public void visitDownloadAction(ActionImpl arg0) {
		// No action required

	}

	@Override
	public void visitEditAction(ActionImpl arg0) {
		// No action required

	}

	@Override
	public void visitFilterParameter(FilterParameter arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitNavigateAction(ActionImpl arg0) {
		// No action required

	}

	@Override
	public void visitNewAction(ActionImpl arg0) {
		// No action required

	}

	@Override
	public void visitOKAction(ActionImpl arg0) {
		// No action required

	}

	@Override
	public void visitParameter(Parameter arg0, boolean arg1, boolean arg2) {
		// No action required

	}

	@Override
	public void visitRemoveAction(ActionImpl arg0) {
		// No action required

	}

	@Override
	public void visitReportAction(ActionImpl arg0) {
		// No action required

	}

	@Override
	public void visitSaveAction(ActionImpl arg0) {
		// No action required

	}

	@Override
	public void visitUploadAction(ActionImpl arg0) {
		// No action required

	}

	@Override
	public void visitZoomOutAction(ActionImpl arg0) {
		// No action required

	}

}
