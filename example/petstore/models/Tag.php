<?php
namespace Petstore\Models;

/**
 * @model Tag
 */
class Tag
{
    /**
     * @property id:long "Unique identifier for the tag"
     */
    public $id;

    /**
     * @property name:string "Friendly name for the tag"
     */
    public $name;

}