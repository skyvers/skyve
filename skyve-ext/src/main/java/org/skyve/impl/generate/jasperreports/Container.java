package org.skyve.impl.generate.jasperreports;

import java.util.ArrayList;
import java.util.List;

/**
 * Recursive generic layout concept (for Skyve view containers)
 * 
 * Mutable layout node used while converting view container structures into
 * absolute Jasper report element coordinates.
 *
 * <p>Each instance tracks local sizing hints and holds either nested child
 * containers or leaf {@link ReportElement} values that are flattened into
 * report bands after layout.
 */
public class Container {
	private ContainerType containerType;

	private Boolean horizontal = Boolean.FALSE;
	private int depth = 0;
	private Integer left = null;
	private Integer top = null;
	private Integer width = null;
	private Integer height = null;
	private Boolean border = Boolean.FALSE;
	private String borderTitle= null;
	
	private Integer pixelWidth = null;
	private Integer percentageWidth =null;
	private Integer responsiveWidth =null;
	private int rows =0;

	private int verticalPosition = 0;
	private boolean filled = false;
	private String printWhenExpression = null;
	
	List<ReportElement> elements;
	List<Container> containers;
	Container parent;
	
	/**
	 * Enumerates supported container layout semantics from the source view model.
	 */
	public static enum ContainerType {
		tab, hbox, vbox, form, column, subreport
	}

	/**
	 * Creates an empty container with no positional or sizing constraints.
	 */
	public Container() {
		horizontal = Boolean.FALSE;
		left = null;
		top = null;
		width = null;
		height = null;
		
		verticalPosition = 0;
		elements = new ArrayList<>();
		containers = new ArrayList<>();
		parent= null;
				
	}
	
	/**
	 * Creates a container with initial position, width hint and orientation.
	 *
	 * @param top The container top offset.
	 * @param left The container left offset.
	 * @param width The initial width hint.
	 * @param horizontal Whether child layout should flow horizontally.
	 * @param containerType The logical container type.
	 */
	public Container(Integer top, Integer left, Integer width, boolean horizontal, ContainerType containerType) {
		this.horizontal = Boolean.valueOf(horizontal);
		this.left = left;
		this.top = top;
		this.width = width;
		this.containerType = containerType;
		this.height = null; //to be calculated
		
		verticalPosition = 0;
		elements = new ArrayList<>();
		containers = new ArrayList<>();
		parent = null;
		
	}
	
	/**
	 * Returns whether this container should render a border.
	 *
	 * @return {@code true} when a border should be rendered.
	 */
	public Boolean getBorder() {
		return border;
	}

	/**
	 * Sets whether this container should render a border.
	 *
	 * @param border The border flag.
	 */
	public void setBorder(Boolean border) {
		this.border = border;
	}

	/**
	 * Returns the optional border title.
	 *
	 * @return The border title text, or {@code null}.
	 */
	public String getBorderTitle() {
		return borderTitle;
	}

	/**
	 * Sets the optional border title.
	 *
	 * @param borderTitle The border title text.
	 */
	public void setBorderTitle(String borderTitle) {
		this.borderTitle = borderTitle;
	}

	/**
	 * Returns the configured row count used by form/column layouts.
	 *
	 * @return The row count.
	 */
	public int getRows() {
		return rows;
	}

	/**
	 * Sets the configured row count used by form/column layouts.
	 *
	 * @param rows The row count.
	 */
	public void setRows(int rows) {
		this.rows = rows;
	}
	
	/**
	 * Returns the fixed pixel width hint.
	 *
	 * @return The pixel width hint, or {@code null}.
	 */
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	/**
	 * Sets the fixed pixel width hint.
	 *
	 * @param pixelWidth The pixel width hint.
	 */
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	/**
	 * Returns the percentage width hint.
	 *
	 * @return The percentage width hint, or {@code null}.
	 */
	public Integer getPercentageWidth() {
		return percentageWidth;
	}

	/**
	 * Sets the percentage width hint.
	 *
	 * @param percentageWidth The percentage width hint.
	 */
	public void setPercentageWidth(Integer percentageWidth) {
		this.percentageWidth = percentageWidth;
	}

	/**
	 * Returns the responsive 12-column width hint.
	 *
	 * @return The responsive width hint, or {@code null}.
	 */
	public Integer getResponsiveWidth() {
		return responsiveWidth;
	}

	/**
	 * Sets the responsive 12-column width hint.
	 *
	 * @param responsiveWidth The responsive width hint.
	 */
	public void setResponsiveWidth(Integer responsiveWidth) {
		this.responsiveWidth = responsiveWidth;
	}

	/**
	 * Indicates whether this container has any rendered content.
	 *
	 * @return {@code true} when this container has filled children or elements.
	 */
	public boolean isFilled() {
		return filled;
	}

	/**
	 * Sets whether this container has any rendered content.
	 *
	 * @param filled The filled flag.
	 */
	public void setFilled(boolean filled) {
		this.filled = filled;
	}

	/**
	 * Returns the running vertical cursor used during layout.
	 *
	 * @return The current vertical cursor position.
	 */
	public int getVerticalPosition() {
		return verticalPosition;
	}

	/**
	 * Sets the running vertical cursor used during layout.
	 *
	 * @param verticalPosition The vertical cursor position.
	 */
	public void setVerticalPosition(int verticalPosition) {
		this.verticalPosition = verticalPosition;
	}
	
	/**
	 * Returns the computed container height.
	 *
	 * @return The computed height, or {@code null} when not yet sized.
	 */
	public Integer getHeight() {
		return height;
	}

	/**
	 * Sets the computed container height.
	 *
	 * @param height The container height.
	 */
	public void setHeight(Integer height) {
		this.height = height;
	}

	/**
	 * Increments the current container height.
	 *
	 * @param height The height delta to add.
	 */
	public void addHeight(@SuppressWarnings("hiding") Integer height){
		if(this.height==null){
			this.height = height;
		} 
		else if(height!=null){
			this.height= Integer.valueOf(this.height.intValue() + height.intValue());
		}
	}

	/**
	 * Increments the current container width.
	 *
	 * @param width The width delta to add.
	 */
	public void addWidth(@SuppressWarnings("hiding") Integer width){
		if(this.width==null){
			this.width = width;
		} 
		else if(width!=null){
			this.width= Integer.valueOf(this.width.intValue() + width.intValue());
		}
	}

	/**
	 * Returns the logical container type.
	 *
	 * @return The container type.
	 */
	public ContainerType  getContainerType() {
		return containerType;
	}

	/**
	 * Sets the logical container type.
	 *
	 * @param containerType The container type.
	 */
	public void setContainerType(ContainerType containerType) {
		this.containerType = containerType;
	}

	/**
	 * Returns whether child layout flows horizontally.
	 *
	 * @return {@code true} when children are laid out left-to-right.
	 */
	public Boolean getHorizontal() {
		return horizontal;
	}

	/**
	 * Sets whether child layout flows horizontally.
	 *
	 * @param horizontal The layout direction flag.
	 */
	public void setHorizontal(Boolean horizontal) {
		this.horizontal = horizontal;
	}

	/**
	 * Returns the absolute left offset.
	 *
	 * @return The left offset.
	 */
	public Integer getLeft() {
		return left;
	}

	/**
	 * Sets the absolute left offset.
	 *
	 * @param left The left offset.
	 */
	public void setLeft(Integer left) {
		this.left = left;
	}
	/**
	 * Returns the optional print-when expression associated with this container.
	 *
	 * @return The print-when expression, or {@code null}.
	 */
	public String getPrintWhenExpression() {
		return printWhenExpression;
	}

	/**
	 * Sets the optional print-when expression associated with this container.
	 *
	 * @param printWhenExpression The print-when expression.
	 */
	public void setPrintWhenExpression(String printWhenExpression) {
		this.printWhenExpression = printWhenExpression;
	}


	/**
	 * Increments the left offset while handling an initially unset value.
	 *
	 * @param left The offset delta to add.
	 */
	public void addLeft(@SuppressWarnings("hiding") Integer left){
		if(this.left==null){
			this.left = left;
		} 
		else if(left!=null){
			this.left = Integer.valueOf(this.left.intValue() + left.intValue());
		}
	}
	
	/**
	 * Returns the absolute top offset.
	 *
	 * @return The top offset.
	 */
	public Integer getTop() {
		return top;
	}

	/**
	 * Sets the absolute top offset.
	 *
	 * @param top The top offset.
	 */
	public void setTop(Integer top) {
		this.top = top;
	}

	/**
	 * Returns the computed container width.
	 *
	 * @return The width, or {@code null} when not yet sized.
	 */
	public Integer getWidth() {
		return width;
	}

	/**
	 * Sets the computed container width.
	 *
	 * @param width The container width.
	 */
	public void setWidth(Integer width) {
		this.width = width;
	}

	/**
	 * Returns the leaf report elements held directly by this container.
	 *
	 * @return Mutable list of direct child elements.
	 */
	public List<ReportElement> getElements() {
		return elements;
	}

	/**
	 * Replaces the leaf report elements held directly by this container.
	 *
	 * @param elements New direct child elements list.
	 */
	public void setElements(List<ReportElement> elements) {
		this.elements = elements;
	}

	/**
	 * Returns nested child containers.
	 *
	 * @return Mutable list of child containers.
	 */
	public List<Container> getContainers() {
		return containers;
	}

	/**
	 * Replaces nested child containers.
	 *
	 * @param containers New child containers list.
	 */
	public void setContainers(List<Container> containers) {
		this.containers = containers;
	}
	
	/**
	 * Returns the nesting depth from the layout root.
	 *
	 * @return The container depth.
	 */
	public int getDepth(){
		return depth;
	}
	
	/**
	 * Sets the nesting depth from the layout root.
	 *
	 * @param depth The container depth.
	 */
	public void setDepth(int depth){
		this.depth = depth;
	}

	/**
	 * Sets the parent container and updates this container depth.
	 *
	 * @param c The parent container.
	 */
	public void setParent(Container c){
		this.parent = c;
		this.depth = c.getDepth()+1;
	}
	
	/**
	 * Returns the parent container.
	 *
	 * @return The parent container, or {@code null} when this is a root.
	 */
	public Container getParent(){
		return parent;
	}
	
	/**
	 * Adds a child container and establishes parent/depth relationships.
	 *
	 * @param c The child container to add.
	 */
	public void addContainer(Container c){
		c.setDepth(depth+1);
		c.setParent(this);
		this.containers.add(c);
	}
}
