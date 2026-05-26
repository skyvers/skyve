package org.skyve.impl.generate.jasperreports;

import java.util.ArrayList;
import java.util.List;

/**
 * Recursive generic layout concept (for Skyve view containers)
 * 
 * @author Robert
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
	
	public static enum ContainerType {
		tab, hbox, vbox, form, column, subreport
	}
	
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
	 * Returns the border.
	 */
	public Boolean getBorder() {
		return border;
	}

	/**
	 * Sets the border.
	 */
	public void setBorder(Boolean border) {
		this.border = border;
	}

	/**
	 * Returns the borderTitle.
	 */
	public String getBorderTitle() {
		return borderTitle;
	}

	/**
	 * Sets the borderTitle.
	 */
	public void setBorderTitle(String borderTitle) {
		this.borderTitle = borderTitle;
	}

	/**
	 * Returns the rows.
	 */
	public int getRows() {
		return rows;
	}

	/**
	 * Sets the rows.
	 */
	public void setRows(int rows) {
		this.rows = rows;
	}
	
	/**
	 * Returns the pixelWidth.
	 */
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	/**
	 * Sets the pixelWidth.
	 */
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	/**
	 * Returns the percentageWidth.
	 */
	public Integer getPercentageWidth() {
		return percentageWidth;
	}

	/**
	 * Sets the percentageWidth.
	 */
	public void setPercentageWidth(Integer percentageWidth) {
		this.percentageWidth = percentageWidth;
	}

	/**
	 * Returns the responsiveWidth.
	 */
	public Integer getResponsiveWidth() {
		return responsiveWidth;
	}

	/**
	 * Sets the responsiveWidth.
	 */
	public void setResponsiveWidth(Integer responsiveWidth) {
		this.responsiveWidth = responsiveWidth;
	}

	/**
	 * Indicates whether isFilled is satisfied.
	 */
	public boolean isFilled() {
		return filled;
	}

	/**
	 * Sets the filled.
	 */
	public void setFilled(boolean filled) {
		this.filled = filled;
	}

	/**
	 * Returns the verticalPosition.
	 */
	public int getVerticalPosition() {
		return verticalPosition;
	}

	/**
	 * Sets the verticalPosition.
	 */
	public void setVerticalPosition(int verticalPosition) {
		this.verticalPosition = verticalPosition;
	}
	
	/**
	 * Returns the height.
	 */
	public Integer getHeight() {
		return height;
	}

	/**
	 * Sets the height.
	 */
	public void setHeight(Integer height) {
		this.height = height;
	}

	/**
	 * Adds a height.
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
	 * Adds a width.
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
	 * Returns the containerType.
	 */
	public ContainerType  getContainerType() {
		return containerType;
	}

	/**
	 * Sets the containerType.
	 */
	public void setContainerType(ContainerType containerType) {
		this.containerType = containerType;
	}

	/**
	 * Returns the horizontal.
	 */
	public Boolean getHorizontal() {
		return horizontal;
	}

	/**
	 * Sets the horizontal.
	 */
	public void setHorizontal(Boolean horizontal) {
		this.horizontal = horizontal;
	}

	/**
	 * Returns the left.
	 */
	public Integer getLeft() {
		return left;
	}

	/**
	 * Sets the left.
	 */
	public void setLeft(Integer left) {
		this.left = left;
	}
	/**
	 * Returns the printWhenExpression.
	 */
	public String getPrintWhenExpression() {
		return printWhenExpression;
	}

	/**
	 * Sets the printWhenExpression.
	 */
	public void setPrintWhenExpression(String printWhenExpression) {
		this.printWhenExpression = printWhenExpression;
	}


	/**
	 * Convenience for adding to possible null 
	 * 
	 * @param left
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
	 * Returns the top.
	 */
	public Integer getTop() {
		return top;
	}

	/**
	 * Sets the top.
	 */
	public void setTop(Integer top) {
		this.top = top;
	}

	/**
	 * Returns the width.
	 */
	public Integer getWidth() {
		return width;
	}

	/**
	 * Sets the width.
	 */
	public void setWidth(Integer width) {
		this.width = width;
	}

	/**
	 * Returns the elements.
	 */
	public List<ReportElement> getElements() {
		return elements;
	}

	/**
	 * Sets the elements.
	 */
	public void setElements(List<ReportElement> elements) {
		this.elements = elements;
	}

	/**
	 * Returns the containers.
	 */
	public List<Container> getContainers() {
		return containers;
	}

	/**
	 * Sets the containers.
	 */
	public void setContainers(List<Container> containers) {
		this.containers = containers;
	}
	
	/**
	 * Returns the depth.
	 */
	public int getDepth(){
		return depth;
	}
	
	/**
	 * Sets the depth.
	 */
	public void setDepth(int depth){
		this.depth = depth;
	}

	/**
	 * Sets the parent.
	 */
	public void setParent(Container c){
		this.parent = c;
		this.depth = c.getDepth()+1;
	}
	
	/**
	 * Returns the parent.
	 */
	public Container getParent(){
		return parent;
	}
	
	/**
	 * Adds a container.
	 */
	public void addContainer(Container c){
		c.setDepth(depth+1);
		c.setParent(this);
		this.containers.add(c);
	}
}
