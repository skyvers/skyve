package org.skyve.impl.generate.jasperreports;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Describes a JasperReports report band (title, column header, detail, summary,
 * etc.) used during programmatic {@code .jrxml} generation.
 */
public class ReportBand {
	@SuppressWarnings("java:S115") // Enum names are JasperReports band names.
	public enum BandType {
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

	@SuppressWarnings("java:S115") // Enum names are JasperReports split type names.
	public enum SplitType {
		immediate,
		prevent,
		stretch;

		@Override
		public @Nonnull String toString() {
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
	
	public @Nullable String getName() {
		return name;
	}
	public void setName(@Nullable String name) {
		this.name = name;
	}
	public @Nullable BandType getBandType() {
		return type;
	}
	public void setBandType(@Nullable BandType type) {
		this.type = type;
	}
	public @Nonnull Integer getHeight() {
		int maxHeight = getNullSafeParent().getDefaultElementHeight().intValue();
		for (ReportElement e : this.getElements()) {
			final Integer elementTop = e.getElementTop();
			final Integer elementHeight = e.getElementHeight();
			if (elementTop != null && elementHeight != null) {
				if ((elementTop.intValue() + elementHeight.intValue()) > maxHeight) {
					maxHeight = elementTop.intValue() + elementHeight.intValue();
				}
			}
		}

		//handle max
		if(height==null || height.intValue()<maxHeight){
			return Integer.valueOf(maxHeight);
		}
		
		return height;
	}
	public void setHeight(@Nullable Integer height) {
		this.height = height;
	}
	public @Nonnull List<ReportElement> getElements() {
		return elements;
	}
	public void setElements(@Nonnull List<ReportElement> elements) {
		this.elements = elements;
	}
	public @Nullable SplitType getSplitType() {
		return splitType;
	}
	public void setSplitType(@Nullable SplitType splitType) {
		this.splitType = splitType;
	}
	public @Nonnull String getJrxml() {
		return Renderer.renderBand(this);
	}
	public @Nullable DesignSpecification getParent() {
		return parent;
	}
	public void setParent(@Nullable DesignSpecification parent) {
		this.parent = parent;
	}

	/**
	 * Returns the design this band has been added to.
	 *
	 * @return The parent design.
	 * @throws IllegalStateException If the band has not been added to a design.
	 */
	@Nonnull DesignSpecification getNullSafeParent() {
		final DesignSpecification result = parent;
		if (result == null) {
			throw new IllegalStateException("Report band " + name + " has not been added to a design");
		}
		return result;
	}

	public @Nullable String getInvisibleConditionName() {
		return invisibleConditionName;
	}


	public void setInvisibleConditionName(@Nullable String invisibleConditionName) {
		this.invisibleConditionName = invisibleConditionName;
	}

	/**
	 * Add element with defaults from band
	 * 
	 * @param tE
	 */
	public void addElement(@Nonnull ReportElement tE) {

		this.getElements().add(tE);
		tE.setParent(this);

		final DesignSpecification design = getNullSafeParent();

		// inherit from band
		if (tE.getElementFontName() == null) {
			tE.setElementFontName(design.getDefaultFontName());
		}

		// font size
		if (tE.getElementFontSize() == null) {
			tE.setElementFontSize(design.getDefaultFontSize());
		}

		// height
		if (tE.getElementHeight() == null) {
			tE.setElementHeight(design.getDefaultElementHeight());
		}

		// dynamic flow
		if (tE.getDynamicFlow() == null) {
			tE.setDynamicFlow(design.getDynamicFlow());
		}

		// border
		if (tE.getElementBorder() == null) {
			tE.setElementBorder(design.getDefaultBorder());
		}

		tE.setBorderLineWidth(design.getDefaultLineWidth());
		tE.setBorderColour(design.getDefaultLineColour());

		tE.setBorderTop(design.getDefaultBorderTop());
		tE.setBorderLeft(design.getDefaultBorderLeft());
		tE.setBorderBottom(design.getDefaultBorderBottom());
		tE.setBorderRight(design.getDefaultBorderRight());

		// padding
		tE.setTopPadding(design.getDefaultCellTopPadding());
		tE.setLeftPadding(design.getDefaultCellLeftPadding());
		tE.setBottomPadding(design.getDefaultCellBottomPadding());
		tE.setRightPadding(design.getDefaultCellRightPadding());
	}


	/**
	 * Spread elements evenly
	 */
	public void spreadElements() {
		if (this.getElements().isEmpty()) {
			return;
		}
		final DesignSpecification design = getNullSafeParent();
		if (design.getWidth() != null) {
			int width = design.getWidth().intValue() / this.getElements().size();
			int left = 0;
			for (ReportElement e : this.getElements()) {
				e.setElementLeft(Integer.valueOf(left));
				// an element without a height is sized as addElement() would size it - with the design default
				Integer elementHeight = e.getElementHeight();
				if (elementHeight == null) {
					elementHeight = design.getDefaultElementHeight();
				}
				if (this.getElements().indexOf(e) == 0) {
					// first
					width = elementHeight.intValue() * 5;
				} else if (this.getElements().indexOf(e) == this.getElements().size() - 1) {
					// last
					width = design.getColumnWidth().intValue() - left;
				} else {
					// other
					width = elementHeight.intValue() * 5;
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
	public void addContainer(@Nonnull Container container) {

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
