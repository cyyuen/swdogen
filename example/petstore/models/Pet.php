<?php
namespace Petstore\Models;

/**
 * @model Pet 
 */
class Pet
{
    /**
     * @property id:double[0.0 - 100.0] "Unique identifier for the Pet"
     */
    public $id;

    /**
     * @property name:string "Friendly name of the pet"
     */
    public $name;

    /**
     * @property category:Category option "Category the pet is in"
     */
    public $category;

    /**
     * @property photoUrls:array[string] option "Image URLs"
     */
    public $photos;

    /**
     * @property tags:array[Tag] option "Tags assigned to this pet"
     */
    public $tags;

    /**
     * @property status:string("available"|"pending"|"sold") option "pet status in the store"
     */
    public $status;

}

