package org.skyve.toolchain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;

import org.apache.maven.plugins.annotations.Parameter;
import org.junit.jupiter.api.Test;

/**
 * Unit tests for SystemDocumentationMojo, focusing on parameter configuration.
 */
class SystemDocumentationMojoTest {

    /**
     * Test that the excludedModules parameter is properly configured with property attribute
     * to support command-line property expressions.
     * 
     * This test verifies that the @Parameter annotation has the correct property attribute
     * that allows users to pass -DexcludedModules=module1,module2 from the command line.
     */
    @Test
    void testExcludedModulesParameterPropertyConfiguration() throws Exception {
        // Use reflection to get the excludedModules field
        Field excludedModulesField = SystemDocumentationMojo.class.getDeclaredField("excludedModules");
        assertNotNull(excludedModulesField, "excludedModules field should exist");
        
        // Get the @Parameter annotation
        Parameter parameterAnnotation = excludedModulesField.getAnnotation(Parameter.class);
        assertNotNull(parameterAnnotation, "excludedModules field should have @Parameter annotation");
        
        // Check that the property attribute is set to "excludedModules"
        String propertyName = parameterAnnotation.property();
        assertEquals("excludedModules", propertyName, 
                    "Parameter should have property='excludedModules' to support command-line properties");
        
        // Verify the field type is String
        assertEquals(String.class, excludedModulesField.getType(), 
                    "excludedModules field should be of type String");
    }
    
    /**
     * Test that other critical parameters maintain their expected configuration.
     * This ensures our change doesn't break existing functionality.
     */
    @Test
    void testOtherParametersConfiguration() throws Exception {
        // Test customer parameter
        Field customerField = SystemDocumentationMojo.class.getDeclaredField("customer");
        assertNotNull(customerField, "customer field should exist");
        
        Parameter customerParamAnnotation = customerField.getAnnotation(Parameter.class);
        assertNotNull(customerParamAnnotation, "customer field should have @Parameter annotation");
        assertTrue(customerParamAnnotation.required(), "customer parameter should be required");
        assertEquals("skyve", customerParamAnnotation.defaultValue(), "customer should have default value 'skyve'");
        
        // Test srcDir parameter
        Field srcDirField = SystemDocumentationMojo.class.getDeclaredField("srcDir");
        assertNotNull(srcDirField, "srcDir field should exist");
        
        Parameter srcDirParamAnnotation = srcDirField.getAnnotation(Parameter.class);
        assertNotNull(srcDirParamAnnotation, "srcDir field should have @Parameter annotation");
        assertTrue(srcDirParamAnnotation.required(), "srcDir parameter should be required");
        assertEquals("src/main/java/", srcDirParamAnnotation.defaultValue(), 
                    "srcDir should have default value 'src/main/java/'");
    }
}