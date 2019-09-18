package org.skyve.metadata.view.model.chart;

import java.util.List;
import java.util.stream.Collectors;

import org.skyve.metadata.view.model.chart.colours.ColourSeries;
import org.skyve.metadata.view.model.chart.colours.RainbowColourSeries;
import org.skyve.persistence.BizQL;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.SQL;

/**
 * Generate the ChartData based on a simple declarative method call chain.
 * The chart is generated from a BizQL, a document query or an SQL and the values are extracted 
 * by position:-
 * Array index 0 is the category and should be a String.
 * Array index 1 is the value and should be a Number.
 * 
 * @author mike
 */
public class TupleChartBuilder {
	private DocumentQuery query;
	private BizQL bizql;
	private SQL sql;
	private String fullyQualifiedJFreeChartPostProcessorClassName;
	private String fullyQualifiedPrimeFacesChartPostProcessorClassName;
	
	/**
	 * Document Query builder.
	 * @param query
	 */
	public TupleChartBuilder with(@SuppressWarnings("hiding") DocumentQuery query) {
		this.query = query;
		return this;
	}
	/**
	 * BizQL builder.
	 * @param bizql
	 */
	public TupleChartBuilder with(@SuppressWarnings("hiding") BizQL bizql) {
		this.bizql = bizql;
		return this;
	}

	/**
	 * SQL builder.
	 * @param sql
	 */
	public TupleChartBuilder with(@SuppressWarnings("hiding") SQL sql) {
		this.sql = sql;
		return this;
	}

	/**
	 * Add a JFreeChart post processor to this chart.
	 * @param fullyQualifiedJFreeChartPostProcessorClassName	The postProcessor to add.
	 */
	public TupleChartBuilder jFreeChartPostProcessorClassName(@SuppressWarnings("hiding") String fullyQualifiedJFreeChartPostProcessorClassName) {
		this.fullyQualifiedJFreeChartPostProcessorClassName = fullyQualifiedJFreeChartPostProcessorClassName;
		return this;
	}

	/**
	 * Add a PrimeFaces Chart post processor to this chart.
	 * @param fullyQualifiedPrimeFacesChartPostProcessorClassName	The postProcessor to add.
	 */
	public TupleChartBuilder primeFacesChartPostProcessorClassName(@SuppressWarnings("hiding") String fullyQualifiedPrimeFacesChartPostProcessorClassName) {
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
		List<Object[]> data = query();
		backgroundColours.setSize(data.size());
		borderColours.setSize(data.size());
		
		ChartData result = new ChartData();
		result.setTitle(title);
		result.setLabel(label);
		result.setBackground(backgroundColours.getCurrent(200));
		result.setBorder(borderColours.getCurrent());
		result.setLabels(data.stream().map(r -> ChartBuilder.label((String) r[0])).collect(Collectors.toList()));
		result.setValues(data.stream().map(r -> (Number) r[1]).collect(Collectors.toList()));

		result.setBackgrounds(backgroundColours.list(200));
		result.setBorders(borderColours.list());
		result.setJFreeChartPostProcessorClassName(fullyQualifiedJFreeChartPostProcessorClassName);
		result.setPrimeFacesChartPostProcessorClassName(fullyQualifiedPrimeFacesChartPostProcessorClassName);
		return result;
	}
	
	private List<Object[]> query() {
		List<Object[]> result = null;
		if (query != null) {
			result = query.tupleResults();
		}
		else if (bizql != null) {
			result = bizql.tupleResults();
		}
		else if (sql != null) {
			result = sql.tupleResults();
		}
		return result;
	}
}
