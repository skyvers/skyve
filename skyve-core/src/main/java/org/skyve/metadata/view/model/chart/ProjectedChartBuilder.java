package org.skyve.metadata.view.model.chart;

import java.util.List;
import java.util.stream.Collectors;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.view.model.chart.colours.ColourSeries;
import org.skyve.metadata.view.model.chart.colours.RainbowColourSeries;
import org.skyve.persistence.BizQL;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.Binder;

/**
 * Generate the ChartData based on a simple declarative method call chain.
 * The chart is generated from a BizQL or a document query and the values are extracted 
 * by the categoryAlias and valueAlias.
 * 
 * @author mike
 */
public class ProjectedChartBuilder {
	private DocumentQuery query;
	private BizQL bizql;
	private String categoryAlias;
	private String valueAlias;
	private String fullyQualifiedJFreeChartPostProcessorClassName;
	private String fullyQualifiedPrimeFacesChartPostProcessorClassName;
	
	/**
	 * DocumentQuery Builder.
	 * @param moduleName
	 * @param documentName
	 */
	public ProjectedChartBuilder with(@SuppressWarnings("hiding") DocumentQuery query) {
		this.query = query;
		return this;
	}
	
	/**
	 * BizQL Builder.
	 * @param bizql
	 */
	public ProjectedChartBuilder with(@SuppressWarnings("hiding") BizQL bizql) {
		this.bizql = bizql;
		return this;
	}
	
	/**
	 * Set the category data field
	 * @param alias
	 */
	public ProjectedChartBuilder category(String alias) {
		categoryAlias = alias;
		return this;
	}
	
	/**
	 * Set the value data field.
	 * @param alias
	 */
	public ProjectedChartBuilder value(String alias) {
		valueAlias = alias;
		return this;
	}
	
	/**
	 * Add a JFreeChart post processor to this chart.
	 * @param fullyQualifiedJFreeChartPostProcessorClassName	The postProcessor to add.
	 */
	public ProjectedChartBuilder jFreeChartPostProcessorClassName(@SuppressWarnings("hiding") String fullyQualifiedJFreeChartPostProcessorClassName) {
		this.fullyQualifiedJFreeChartPostProcessorClassName = fullyQualifiedJFreeChartPostProcessorClassName;
		return this;
	}

	/**
	 * Add a PrimeFaces Chart post processor to this chart.
	 * @param fullyQualifiedPrimeFacesChartPostProcessorClassName	The postProcessor to add.
	 */
	public ProjectedChartBuilder primeFacesChartPostProcessorClassName(@SuppressWarnings("hiding") String fullyQualifiedPrimeFacesChartPostProcessorClassName) {
		this.fullyQualifiedPrimeFacesChartPostProcessorClassName = fullyQualifiedPrimeFacesChartPostProcessorClassName;
		return this;
	}

	/**
	 * Build the ChartData.
	 * @param label	The data set label.
	 * @return	The ChartData.
	 */
	public ChartData build(String label) {
		return build(null, label);
	}

	/**
	 * Build the ChartData.
	 * @param title The chart title.
	 * @param label	The data set label.
	 * @return	The ChartData.
	 */
	public ChartData build(String title, String label) {
		return build(new RainbowColourSeries(), new RainbowColourSeries(), title, label);
	}
	
	/**
	 * Build the ChartData using defined ColourSeries.
	 * @param backgroundColours
	 * @param borderColours
	 * @param label
	 * @return	The ChartData.
	 */
	public ChartData build(ColourSeries backgroundColours,
							ColourSeries borderColours,
							String label) {
		return build(backgroundColours, borderColours, null, label);
	}
	
	/**
	 * Build the ChartData using defined ColourSeries.
	 * @param backgroundColours
	 * @param borderColours
	 * @param title
	 * @param label
	 * @return	The ChartData.
	 */
	public ChartData build(ColourSeries backgroundColours,
							ColourSeries borderColours,
							String title,
							String label) {
		List<Bean> data = query();
		backgroundColours.setSize(data.size());
		borderColours.setSize(data.size());
		
		ChartData result = new ChartData();
		result.setTitle(title);
		result.setLabel(label);
		result.setBackground(backgroundColours.getCurrent(200));
		result.setBorder(borderColours.getCurrent());
		Customer c = CORE.getCustomer();
		result.setLabels(data.stream().map(r -> ChartBuilder.label(Binder.getDisplay(c, r, categoryAlias))).collect(Collectors.toList()));
		result.setValues(data.stream().map(r -> (Number) Binder.get(r, valueAlias)).collect(Collectors.toList()));

		result.setBackgrounds(backgroundColours.list(200));
		result.setBorders(borderColours.list());
		result.setJFreeChartPostProcessorClassName(fullyQualifiedJFreeChartPostProcessorClassName);
		result.setPrimeFacesChartPostProcessorClassName(fullyQualifiedPrimeFacesChartPostProcessorClassName);
		return result;
	}
	
	private List<Bean> query() {
		List<Bean> result = null;
		if (query != null) {
			result = query.projectedResults();
		}
		else if (bizql != null) {
			result = bizql.projectedResults();
		}
		return result;
	}
}
