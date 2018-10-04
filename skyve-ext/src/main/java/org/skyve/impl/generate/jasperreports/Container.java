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

	private Boolean horizontal = false;
	private int depth = 0;
	private Integer left = null;
	private Integer top = null;
	private Integer width = null;
	private Integer height = null;
	private Boolean border = false;
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
		horizontal = false;
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
		this.horizontal = horizontal;
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
	
	public Boolean getBorder() {
		return border;
	}

	public void setBorder(Boolean border) {
		this.border = border;
	}

	public String getBorderTitle() {
		return borderTitle;
	}

	public void setBorderTitle(String borderTitle) {
		this.borderTitle = borderTitle;
	}

	public int getRows() {
		return rows;
	}

	public void setRows(int rows) {
		this.rows = rows;
	}
	
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	public Integer getPercentageWidth() {
		return percentageWidth;
	}

	public void setPercentageWidth(Integer percentageWidth) {
		this.percentageWidth = percentageWidth;
	}

	public Integer getResponsiveWidth() {
		return responsiveWidth;
	}

	public void setResponsiveWidth(Integer responsiveWidth) {
		this.responsiveWidth = responsiveWidth;
	}

	public boolean isFilled() {
		return filled;
	}

	public void setFilled(boolean filled) {
		this.filled = filled;
	}

	public int getVerticalPosition() {
		return verticalPosition;
	}

	public void setVerticalPosition(int verticalPosition) {
		this.verticalPosition = verticalPosition;
	}
	
	public Integer getHeight() {
		return height;
	}

	public void setHeight(Integer height) {
		this.height = height;
	}

	public void addHeight(Integer height){
		if(this.height==null){
			this.height = height;
		} 
		else if(height!=null){
			this.height= this.height.intValue() + height.intValue();
		}
	}

	public void addWidth(Integer width){
		if(this.width==null){
			this.width = width;
		} 
		else if(width!=null){
			this.width= this.width.intValue() + width.intValue();
		}
	}

	public ContainerType  getContainerType() {
		return containerType;
	}

	public void setContainerType(ContainerType containerType) {
		this.containerType = containerType;
	}

	public Boolean getHorizontal() {
		return horizontal;
	}

	public void setHorizontal(Boolean horizontal) {
		this.horizontal = horizontal;
	}

	public Integer getLeft() {
		return left;
	}

	public void setLeft(Integer left) {
		this.left = left;
	}
	public String getPrintWhenExpression() {
		return printWhenExpression;
	}

	public void setPrintWhenExpression(String printWhenExpression) {
		this.printWhenExpression = printWhenExpression;
	}


	/**
	 * Convenience for adding to possible null 
	 * 
	 * @param left
	 */
	public void addLeft(Integer left){
		if(this.left==null){
			this.left = left;
		} 
		else if(left!=null){
			this.left = this.left.intValue() + left.intValue();
		}
	}
	
	public Integer getTop() {
		return top;
	}

	public void setTop(Integer top) {
		this.top = top;
	}

	public Integer getWidth() {
		return width;
	}

	public void setWidth(Integer width) {
		this.width = width;
	}

	public List<ReportElement> getElements() {
		return elements;
	}

	public void setElements(List<ReportElement> elements) {
		this.elements = elements;
	}

	public List<Container> getContainers() {
		return containers;
	}

	public void setContainers(List<Container> containers) {
		this.containers = containers;
	}
	
	public int getDepth(){
		return depth;
	}
	
	public void setDepth(int depth){
		this.depth = depth;
	}

	public void setParent(Container c){
		this.parent = c;
		this.depth = c.getDepth()+1;
	}
	
	public Container getParent(){
		return parent;
	}
	
	public void addContainer(Container c){
		c.setDepth(depth+1);
		c.setParent(this);
		this.containers.add(c);
	}
	
}
