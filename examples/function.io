struct Car
    license_plate: str
    co2_per_km: i32

impl Car

    display()
        print self.license_plate
    
    energy_usage() -> i32
        return self.co2_per_km

display_car(
    car: Car
) -> str

    car.display()
    print car.energy_usage()

    return car.license_plate

main()
    var car = Car { 
        license_plate: "GR33N", 
        co2_per_km: 0,
    }
    print display_car(car)

main()