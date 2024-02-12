package modules;

/**
 * This class is used to bring in the "modules" package to scan for injection.
 * WeldProvider adds this package by referencing this class.
 * Adding the java.lang.Package equivalent of the modules package is not possible given the 
 * different combinations of class loader hierarchies used (ie test classloader can't get the package)
 * @author mike
 */
public class WeldMarker {
	// nothing to see here.
}
