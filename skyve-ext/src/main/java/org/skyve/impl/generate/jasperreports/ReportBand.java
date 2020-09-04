package org.skyve.impl.generate.jasperreports;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;

public class ReportBand {
	public static enum BandType {
		background,
		title,
		pageHeader,
		columnHeader,
		detail,
		columnFooter,
		pageFooter,
		lastPageFooter,
		summary,
		noData
	}

	public static enum SplitType {
		immediate,
		prevent,
		stretch;

		@Override
		public String toString() {
			return StringUtils.capitalize(super.toString());
		}
	}

	/**
	 * Name
	 **/
	private String name;
	/**
	 * Type
	 **/
	private BandType type;
	/**
	 * Height
	 **/
	private Integer height;
	/**
	 * Elements
	 **/
	private List<ReportElement> elements = new ArrayList<>();
	/**
	 * Split Type
	 **/
	private SplitType splitType;
	private String invisibleConditionName;
	private DesignSpecification parent;	
	
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public BandType getBandType() {
		return type;
	}
	public void setBandType(BandType type) {
		this.type = type;
	}
	public Integer getHeight() {
		int maxHeight = this.getParent().getDefaultElementHeight().intValue();
		for (ReportElement e : this.getElements()) {
			if (e.getElementTop() != null && e.getElementHeight() != null) {
				if ((e.getElementTop().intValue() + e.getElementHeight().intValue()) > maxHeight) {
					maxHeight = e.getElementTop().intValue() + e.getElementHeight().intValue();
				}
			}
		}

		//handle max
		if(height==null || height.intValue()<maxHeight){
			return Integer.valueOf(maxHeight);
		}
		
		return height;
	}
	public void setHeight(Integer height) {
		this.height = height;
	}
	public List<ReportElement> getElements() {
		return elements;
	}
	public void setElements(List<ReportElement> elements) {
		this.elements = elements;
	}
	public SplitType getSplitType() {
		return splitType;
	}
	public void setSplitType(SplitType splitType) {
		this.splitType = splitType;
	}
	public String getJrxml() {
		return Renderer.renderBand(this);
	}
	public DesignSpecification getParent() {
		return parent;
	}
	public void setParent(DesignSpecification parent) {
		this.parent = parent;
	}

	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}


	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = invisibleConditionName;
	}

	/**
	 * Add element with defaults from band
	 * 
	 * @param tE
	 */
	public void addElement(ReportElement tE) {

		this.getElements().add(tE);
		tE.setParent(this);

		// inherit from band
		if (tE.getElementFontName() == null) {
			tE.setElementFontName(this.getParent().getDefaultFontName());
		}

		// font size
		if (tE.getElementFontSize() == null) {
			tE.setElementFontSize(this.getParent().getDefaultFontSize());
		}

		// height
		if (tE.getElementHeight() == null) {
			tE.setElementHeight(this.getParent().getDefaultElementHeight());
		}

		// dynamic flow
		if (tE.getDynamicFlow() == null) {
			tE.setDynamicFlow(this.getParent().getDynamicFlow());
		}

		// border
		if (tE.getElementBorder() == null) {
			tE.setElementBorder(this.getParent().getDefaultBorder());
		}

		tE.setBorderLineWidth(this.getParent().getDefaultLineWidth());
		tE.setBorderColour(this.getParent().getDefaultLineColour());

		tE.setBorderTop(this.getParent().getDefaultBorderTop());
		tE.setBorderLeft(this.getParent().getDefaultBorderLeft());
		tE.setBorderBottom(this.getParent().getDefaultBorderBottom());
		tE.setBorderRight(this.getParent().getDefaultBorderRight());

		// padding
		tE.setTopPadding(this.getParent().getDefaultCellTopPadding());
		tE.setLeftPadding(this.getParent().getDefaultCellLeftPadding());
		tE.setBottomPadding(this.getParent().getDefaultCellBottomPadding());
		tE.setRightPadding(this.getParent().getDefaultCellRightPadding());
	}


	/**
	 * Spread elements evenly
	 */
	public void spreadElements() {
		if (this.getElements().isEmpty()) {
			return;
		} else if (this.getParent().getWidth() != null) {
			int width = this.getParent().getWidth().intValue() / this.getElements().size();
			int left = 0;
			for (ReportElement e : this.getElements()) {
				e.setElementLeft(Integer.valueOf(left));
				if (this.getElements().indexOf(e) == 0) {
					// first
					width = e.getElementHeight().intValue() * 5;
				} else if (this.getElements().indexOf(e) == this.getElements().size() - 1) {
					// last
					width = this.getParent().getColumnWidth().intValue() - left;
				} else {
					// other
					width = e.getElementHeight().intValue() * 5;
				}
				e.setElementWidth(Integer.valueOf(width));
				left = left + width;
			}
		}
	}

	/**
	 * Add a container to the band, recursively through child containers to add all report elements
	 * 
	 * @param band
	 * @param container
	 */
	public void addContainer(Container container) {

		// and add all elements of the top level child
		for (ReportElement e : container.getElements()) {
			this.addElement( e);
		}
		// recursively add all children
		for (Container c : container.getContainers()) {
			this.addContainer(c);
		}
	}

	
}
